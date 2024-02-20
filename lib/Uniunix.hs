{-# LANGUAGE LambdaCase #-}
module Uniunix where

import System.Environment
import Data.Functor
import Data.List
import Control.Monad
import Control.Applicative
import System.Exit
import System.IO
import Data.Foldable
import Control.Arrow((>>>))
import Data.Function
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import qualified Text.Parsec as P

usageMessage :: [String]
usageMessage = 
  ["head - output the first part of files"
  ,"usage: head [OPTION]... [FILE]..."]

helpMessage :: [String]
helpMessage =
  usageMessage ++
  ["Print the first 10 lines of each FILE to standard output.  With more than one FILE, precede each with a header giving the file name."
  ,""
  ,"With no FILE, or when FILE is -, read standard input."
  ,""
  ,"Mandatory arguments to long options are mandatory for short options too."
  ,""
  ,"-c, --bytes=[-]NUM"
  ,"print the first NUM bytes of each file; with the leading '-', print all but the last NUM bytes of each file"
  ,""
  ,"-m, --chars=[-]NUM"
  ,"print the first NUM characters of each file; with the leading '-' print all but the last NUM bytes of each file"
  ,""
  ,"-n, --lines=[-]NUM"
  ,"print the first NUM lines instead of the first 10; with the leading '-', print all but the last NUM lines of each file"
  ,""
  ,"-q, --quiet, --silent"
  ,"never print headers giving file names"
  ,""
  ,"-v, --verbose"
  ,"always print headers giving file names"
  ,""
  ,"-z, --zero-terminated"
  ,"line delimiter is NUL, not newline"
  ,""
  ,"--help display this help and exit"
  ,""
  ,"--version"
  ,"output version information and exit"
  ,""
  ,"NUM  may have a multiplier suffix: b 512, kB 1000, K 1024, MB 1000*1000, M 1024*1024, GB 1000*1000*1000, G 1024*1024*1024"
  ,"Binary prefixes can be used, too: KiB=K, MiB=M, and so on."]

data OutputType =
    OUnknown
  | OBytes Integer
  | OChars Integer
  | OLines Integer
  deriving (Eq)

data ParserState = ParserState {
  outputType :: !OutputType,
  parseErrors :: ![String],
  printHelp :: !Bool,
  printVersion :: !Bool,
  zeroTerminated :: !Bool,
  beQuiet :: !Bool,
  beVerbose :: !Bool,
  files :: ![FilePath]
}

defaultParserState :: ParserState
defaultParserState = ParserState {
  outputType = OUnknown,
  parseErrors = [],
  printHelp = False,
  printVersion = False,
  zeroTerminated = False,
  beQuiet = False,
  beVerbose = False,
  files = []
}

addError :: String -> ParserState -> ParserState
addError err state = state { parseErrors = err : parseErrors state }

mutuallyExclusiveError :: String
mutuallyExclusiveError =
  "error: --bytes, --chars and --lines are mutually exclusive, but more than one of these options were specified"

parse' parser = P.parse parser "" >>> first show

suffixParser :: P.Stream s m Char => P.ParsecT s u m (Integer -> Integer)
suffixParser =
      P.string "b"  $> (*) (2 ^ 9)
  <|> P.string "kB" $> (*) (10 ^ 3)
  <|>   bstring "K" $> (*) (2 ^ 10)
  <|> P.string "MB" $> (*) (10 ^ 6)
  <|>   bstring "M" $> (*) (2 ^ 20)
  <|> P.string "GB" $> (*) (10 ^ 9)
  <|>   bstring "G" $> (*) (2 ^ 30)
  where bstring s = P.string s *> P.optional (P.string "iB")



invalidNumber :: String -> String
invalidNumber n = "error: invalid numeric value: '" ++ n ++ "'"

unexpectedEOA :: String
unexpectedEOA = "error: expected a number argument, got EOF"

verboseAndQuietError :: String
verboseAndQuietError = "error: --verbose and --quiet are mutually exclusive"

parseTypeArg :: (String -> Either String Integer) -> (Integer -> OutputType) -> [String] -> ParserState -> ([String], ParserState)
parseTypeArg parseNumber cons opts state
  | outputType state /= OUnknown = (opts, addError mutuallyExclusiveError state)
  | otherwise =
    case opts of
      num:opts' -> state & buildType parseNumber cons num & (opts',)
      [] -> (opts, addError unexpectedEOA state)

buildType :: (String -> Either String Integer) -> (Integer -> OutputType) -> String -> ParserState -> ParserState
buildType parseNumber cons num state =
  case parseNumber num of
    Right int -> state { outputType = cons int }
    Left _ -> addError (invalidNumber num) state

parseOptions :: (String -> Either String Integer) -> [String] -> ParserState -> ParserState
parseOptions _ [] state@(ParserState { outputType = OUnknown }) = state { outputType = OLines 10 }
parseOptions _ [] state = state
parseOptions parseNumber (opt:opts) state
  | opt == "-c" = state & parseTypeArg parseNumber OBytes opts & uncurry (parseOptions parseNumber)
  | opt == "-m" = state & parseTypeArg parseNumber OChars opts & uncurry (parseOptions parseNumber)
  | opt == "-n" = state & parseTypeArg parseNumber OLines opts & uncurry (parseOptions parseNumber)
  | optnm == "--bytes" = state & buildType parseNumber OBytes optarg & parseOptions parseNumber opts
  | optnm == "--chars" = state & buildType parseNumber OChars optarg & parseOptions parseNumber opts
  | optnm == "--lines" = state & buildType parseNumber OLines optarg & parseOptions parseNumber opts
  | opt == "--help" = state { printHelp = True } & parseOptions parseNumber opts
  | opt == "--version" = state { printVersion = True } & parseOptions parseNumber opts
  | opt `elem` ["--verbose", "-v"] =
    if beQuiet state then 
      addError verboseAndQuietError state & parseOptions parseNumber opts
    else
      state { beVerbose = True } & parseOptions parseNumber opts
  | opt `elem` ["--silent", "--quiet", "-q"] =
    if beVerbose state then
      addError verboseAndQuietError state & parseOptions parseNumber opts
    else
      state { beQuiet = True } & parseOptions parseNumber opts
  | opt `elem` ["--zero-terminated", "-z"] = state { zeroTerminated = True } & parseOptions parseNumber opts
  | otherwise = state { files = opt : files state } & parseOptions parseNumber opts
  where (optnm, optarg) = opt & span (/='=') & second (drop 1)

getFileHandle :: OutputType -> String -> IO Handle
getFileHandle (OBytes _) =
  \case "-" -> return stdin
        path -> openBinaryFile path ReadMode
getFileHandle _ =
  \case "-" -> return stdin
        path -> openFile path ReadMode

printFile :: Integral i => (Handle -> IO a) -> (a -> IO ()) -> (i -> a -> a) -> i -> Handle -> IO ()
printFile reader writer transform count file =
      reader file
  >>= writer . transform count

byteStringGenericTake :: Integral i => i -> BSL.ByteString -> BSL.ByteString
byteStringGenericTake i = if i > fromIntegral (maxBound :: Int) || i < fromIntegral (minBound :: Int)
                          then BSL.unpack >>> genericTake i >>> BSL.pack
                          else BSL.take (fromIntegral i)
byteStringGenericDrop :: Integral i => i -> BSL.ByteString -> BSL.ByteString
byteStringGenericDrop i = if i > fromIntegral (maxBound :: Int) || i < fromIntegral (minBound :: Int)
                          then BSL.unpack >>> genericDrop i >>> BSL.pack
                          else BSL.drop (fromIntegral i)
byteStringGenericLength :: Integral i => BSL.ByteString -> i
byteStringGenericLength str = if BSL.length str < 0
                              then str & BSL.unpack & genericLength
                              else str & BSL.length & fromIntegral
processFiles :: (OutputType -> FilePath -> IO ()) -> ParserState -> IO ()
processFiles ioAction (ParserState { files = [] }) = ioAction (OLines 10) "-"
processFiles ioAction state@(ParserState { beVerbose = False, files = [file] }) = ioAction (outputType state) file
processFiles ioAction state = for_ (files state) $ \file -> do
  if not (beQuiet state) then
    putStrLn ("==> " ++ file ++ " <==")
  else
    return ()
  ioAction (outputType state) file <|> exitWith (ExitFailure 1)

runUniunix :: (String -> Either String Integer) -> (OutputType -> FilePath -> IO ()) -> IO ()
runUniunix parseNumber ioAction = do
  state <- getArgs <&> filter (not . null) <&> \args -> parseOptions parseNumber args defaultParserState
  case (parseErrors state, printHelp state, printVersion state) of
    ([], True, _) -> traverse_ putStrLn helpMessage
    ([], _, True) -> putStrLn "0.1.0.0"
    ([], _, _)    -> processFiles ioAction state
    (errs, _, _)  -> traverse_ putStrLn (reverse errs)
                  >> traverse_ putStrLn usageMessage
                  >> exitWith (ExitFailure 1)
