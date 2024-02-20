
import qualified Text.Parsec as P
import Data.List
import qualified Data.ByteString.Lazy as BSL
import System.IO
import Control.Monad
import Data.Functor
import Data.Function
import Uniunix

parseNumber :: String -> Either String Integer
parseNumber = ( P.optionMaybe (P.char '+')
            <&> maybe id (const negate)   )
            >>= (\neg -> neg . read <$> P.many1 P.digit)
            >>= (\i -> P.optionMaybe suffixParser <&> maybe i (\f -> f i))
            & parse'

takeTail :: Integral i => (a -> i) -> (i -> a -> a) -> i -> a -> a
takeTail len drop' i list =
  if i >= 0 then
    drop' (len list - i) list
  else
    drop' (-i) list
    

putFileTail :: OutputType -> FilePath -> IO ()
putFileTail otype@(OBytes b) = getFileHandle otype >=> printFile (BSL.hGetContents) BSL.putStr (takeTail byteStringGenericLength byteStringGenericDrop) b
putFileTail otype@(OChars c) = getFileHandle otype >=> printFile hGetContents putStr (takeTail genericLength genericDrop) c
putFileTail otype@(OLines l) = getFileHandle otype >=> printFile (hGetContents >=> return . lines) (\ls -> putStr (unlines ls)) (takeTail genericLength genericDrop) l
putFileTail OUnknown = error "putFileTail should've never received OUnknown"

main :: IO ()
main = runUniunix parseNumber putFileTail
