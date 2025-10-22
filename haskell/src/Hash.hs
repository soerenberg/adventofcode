module Hash (strMD5) where
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.UTF8 as BLU

strMD5 :: String -> String
strMD5 = show . md5 . BLU.fromString
