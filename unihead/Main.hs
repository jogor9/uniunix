import Control.Monad
import Data.ByteString.Lazy qualified as BSL
import Data.Function
import Data.Functor
import Data.List
import System.IO
import Text.Parsec qualified as P
import Uniunix

parseNumber :: String -> Either String Integer
parseNumber =
  P.option id (P.char '-' $> negate)
    >>= (\neg -> neg . read <$> P.many1 P.digit)
    >>= (\i -> P.optionMaybe suffixParser <&> maybe i (\f -> f i))
    & parse'

takeHead :: (Integral i) => (a -> i) -> (i -> a -> a) -> i -> a -> a
takeHead len take' i list =
  if i >= 0
    then
      take' i list
    else
      take' (len list + i) list

putFileHead :: OutputType -> FilePath -> IO ()
putFileHead otype@(OBytes b) =
  getFileHandle otype
    >=> printFile
      BSL.hGetContents
      BSL.putStr
      (takeHead byteStringGenericLength byteStringGenericTake)
      b
putFileHead otype@(OChars c) =
  getFileHandle otype
    >=> printFile
      hGetContents
      putStr
      (takeHead genericLength genericTake)
      c
putFileHead otype@(OLines l) =
  getFileHandle otype
    >=> printFile
      (hGetContents >=> return . lines)
      (putStr . unlines)
      (takeHead genericLength genericTake)
      l
putFileHead OUnknown = error "putFileHead should've never received OUnknown"

main :: IO ()
main = runUniunix parseNumber putFileHead
