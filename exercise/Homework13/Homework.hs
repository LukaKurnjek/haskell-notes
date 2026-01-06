import Data.ByteString as BS
import Data.Binary as Bin

{-
Create a function that converts the binary representation of a ByteString into an Integer using a base 256 conversion.

Example: we can convert [4,255,50] in base 256 to base 10 as 

                                50*(256)^0 + 255*(256)^1 + 4*(256)^2. 

Hint: since Haskell byte strings are a list of Word8 (base 256 integers) (see the toInteger :: Word8 -> Integer). 
-}

byteStringToInteger :: BS.ByteString -> Integer

{-
Create a function that converts Integers into the binary representation using a base 256 conversion.
For simplicity we will not represent the parity of the integers (the negative integers). 
You can use the abs function to make all inputs positive.

Example: we can convert 3000000 in base 10 to base 256 as 

        [((300000 `div` 256) `div` 256) `mod` 256,(300000 `div` 256) `mod` 256, 300000 `mod` 256]. 

Hint: since Haskell byte strings are a list of Word8 (base 256 integers) (see the toEnum :: Int -> Word8). 
-}

integerToByteString :: Integer -> BS.ByteString
