
import GHC.Word (Word8)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as BS

bytes :: [Word8]
bytes = [199,191]

byteString :: BS.ByteString
byteString = BS.pack bytes

bytesText :: T.Text 
bytesText = TE.decodeUtf8 byteString

bytesChar :: [Char]
bytesChar = BC.unpack byteString

bytesChar2Bytes :: [Word8]
bytesChar2Bytes = BS.unpack $ BC.pack bytesChar

main :: IO ()
main = do
  TIO.putStrLn bytesText -- prints ǿ
  putStrLn bytesChar -- prints Ç¿
  print bytesChar2Bytes -- prints [199,191]