
import qualified Text.Parsec as P
import Data.List
import qualified Data.ByteString.Lazy as BSL
import Control.Monad
import Data.Functor
import System.IO
import Data.Function
import Uniunix

parseNumber :: String -> Either String Integer
parseNumber = ( P.optionMaybe (P.char '-')
            <&> maybe id (const negate)   )
            >>= (\neg -> neg . read <$> P.many1 P.digit)
            >>= (\i -> P.optionMaybe suffixParser <&> maybe i (\f -> f i))
            & parse'

takeHead :: Integral i => (a -> i) -> (i -> a -> a) -> i -> a -> a
takeHead len take' i list =
  if i >= 0 then
    take' i list
  else
    take' (len list + i) list

putFileHead :: OutputType -> FilePath -> IO ()
putFileHead otype@(OBytes b) = getFileHandle otype >=> printFile (BSL.hGetContents) BSL.putStr (takeHead byteStringGenericLength byteStringGenericTake) b
putFileHead otype@(OChars c) = getFileHandle otype >=> printFile hGetContents putStr (takeHead genericLength genericTake) c
putFileHead otype@(OLines l) = getFileHandle otype >=> printFile (hGetContents >=> return . lines) (\ls -> putStr (unlines ls)) (takeHead genericLength genericTake) l
putFileHead OUnknown = error "putFileHead should've never received OUnknown"

main :: IO ()
main = runUniunix parseNumber putFileHead
