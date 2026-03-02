
import GHC.Word (Word8)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as BS

{- Enocding [255] (last possible byte) would work for [Char], but 
   not for T.Text which handles only up to [127] for single byte.
   The following two bytes are displayed as different characters 
   when decoding them as T.Text and [Char]. -}

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

{-- Up to byte [127] charters get the same encoding for both formats. 
    From [128] to [255] the econding differs. For charaters that are 
    above the 255 unicode character list as example €, Char encoding 
    does not encode correctly - the last putStrLn line gives a different 
    character (https://en.wikipedia.org/wiki/List_of_Unicode_characters) --}

aChar :: BS.ByteString
aChar = BC.pack "€" 

aText :: BS.ByteString
aText = TE.encodeUtf8 $ T.pack "€"

aCharWord8 :: [Word8]
aCharWord8 = BS.unpack aChar

aTextWord8 :: [Word8]
aTextWord8 = BS.unpack aText

main :: IO ()
main = do
  TIO.putStrLn bytesText -- prints ǿ
  putStrLn bytesChar -- prints Ç¿
  print bytesChar2Bytes -- prints [199,191]
  print byteString -- prints "\199\191"
  print aCharWord8 -- prints [172]
  print aTextWord8 -- prints [226,130,172]
  putStrLn $ BC.unpack $ BS.pack aCharWord8 -- "¬"
