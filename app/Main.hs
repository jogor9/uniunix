{-# LANGUAGE LambdaCase #-}
module Main(main) where

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


parseNumber :: String -> Either String Integer
parseNumber = ( P.optionMaybe (P.char '-')
            <&> maybe id (const negate)   )
            >>= (\neg -> neg . read <$> P.many1 P.digit)
            >>= (\i -> P.optionMaybe suffixParser <&> maybe i (\f -> f i))
            & parse'

invalidNumber :: String -> String
invalidNumber n = "error: invalid numeric value: '" ++ n ++ "'"

unexpectedEOA :: String
unexpectedEOA = "error: expected a number argument, got EOF"

verboseAndQuietError :: String
verboseAndQuietError = "error: --verbose and --quiet are mutually exclusive"

parseTypeArg :: (Integer -> OutputType) -> [String] -> ParserState -> ([String], ParserState)
parseTypeArg cons opts state
  | outputType state /= OUnknown = (opts, addError mutuallyExclusiveError state)
  | otherwise =
    case opts of
      num:opts' -> state & buildType cons num & (opts',)
      [] -> (opts, addError unexpectedEOA state)

buildType :: (Integer -> OutputType) -> String -> ParserState -> ParserState
buildType cons num state =
  case parseNumber num of
    Right int -> state { outputType = cons int }
    Left _ -> addError (invalidNumber num) state

parseOptions :: [String] -> ParserState -> ParserState
parseOptions [] state@(ParserState { outputType = OUnknown }) = state { outputType = OLines 10 }
parseOptions [] state = state
parseOptions (opt:opts) state
  | opt == "-c" = state & parseTypeArg OBytes opts & uncurry parseOptions
  | opt == "-m" = state & parseTypeArg OChars opts & uncurry parseOptions
  | opt == "-n" = state & parseTypeArg OLines opts & uncurry parseOptions
  | optnm == "--bytes" = state & buildType OBytes optarg & parseOptions opts
  | optnm == "--chars" = state & buildType OChars optarg & parseOptions opts
  | optnm == "--lines" = state & buildType OLines optarg & parseOptions opts
  | opt == "--help" = state { printHelp = True } & parseOptions opts
  | opt == "--version" = state { printVersion = True } & parseOptions opts
  | opt `elem` ["--verbose", "-v"] =
    if beQuiet state then 
      addError verboseAndQuietError state & parseOptions opts
    else
      state { beVerbose = True } & parseOptions opts
  | opt `elem` ["--silent", "--quiet", "-q"] =
    if beVerbose state then
      addError verboseAndQuietError state & parseOptions opts
    else
      state { beQuiet = True } & parseOptions opts
  | opt `elem` ["--zero-terminated", "-z"] = state { zeroTerminated = True } & parseOptions opts
  | otherwise = state { files = opt : files state } & parseOptions opts
  where (optnm, optarg) = opt & span (/='=') & second (drop 1)

getFileHandle :: OutputType -> String -> IO Handle
getFileHandle (OBytes _) =
  \case "-" -> return stdin
        path -> openBinaryFile path ReadMode
getFileHandle _ =
  \case "-" -> return stdin
        path -> openFile path ReadMode

printFile :: Integral i => (Handle -> IO a) -> (a -> IO ()) -> (i -> a -> a) -> (a -> i) -> i -> Handle -> IO ()
printFile reader writer transform len count file =
      reader file
  >>= writer . \str -> if count < 0 then transform (len str + count) str
                                    else transform count str

byteStringGenericTake :: Integral i => i -> BSL.ByteString -> BSL.ByteString
byteStringGenericTake i = if i > fromIntegral (maxBound :: Int) || i < fromIntegral (minBound :: Int)
                          then BSL.unpack >>> genericTake i >>> BSL.pack
                          else BSL.take (fromIntegral i)
byteStringGenericLength :: Integral i => BSL.ByteString -> i
byteStringGenericLength str = if BSL.length str < 0
                              then str & BSL.unpack & genericLength
                              else str & BSL.length & fromIntegral
putFileHead :: OutputType -> FilePath -> IO ()
putFileHead otype@(OBytes b) = getFileHandle otype >=> printFile (BSL.hGetContents) BSL.putStr byteStringGenericTake byteStringGenericLength b
putFileHead otype@(OChars c) = getFileHandle otype >=> printFile hGetContents putStr genericTake genericLength c
putFileHead otype@(OLines l) = getFileHandle otype >=> printFile (hGetContents >=> return . lines) (\ls -> putStr (unlines ls)) genericTake genericLength l
putFileHead OUnknown = error "putFileHead should've never received OUnknown"

processFiles :: ParserState -> IO ()
processFiles (ParserState { files = [] }) = putFileHead (OLines 10) "-"
processFiles state@(ParserState { beVerbose = False, files = [file] }) = putFileHead (outputType state) file
processFiles state = for_ (files state) $ \file -> do
  if not (beQuiet state) then
    putStrLn ("==> " ++ file ++ " <==")
  else
    return ()
  putFileHead (outputType state) file <|> exitWith (ExitFailure 1)

main :: IO ()
main = do
  state <- getArgs <&> filter (not . null) <&> \args -> parseOptions args defaultParserState
  case (parseErrors state, printHelp state, printVersion state) of
    ([], True, _) -> traverse_ putStrLn helpMessage
    ([], _, True) -> putStrLn "0.1.0.0"
    ([], _, _)    -> processFiles state
    (errs, _, _)  -> traverse_ putStrLn (reverse errs)
                  >> traverse_ putStrLn usageMessage
                  >> exitWith (ExitFailure 1)

