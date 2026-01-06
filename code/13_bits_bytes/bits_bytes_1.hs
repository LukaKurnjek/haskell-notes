import System.IO
import System.CPUTime
import Text.Printf

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import qualified Data.Text.IO as TIO
import qualified Data.Text as T

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "Computation time: %0.4f sec\n" (diff :: Double)
    putStrLn ""
    return v

main :: IO ()
main = do
    fileContent <- readFile "./bible.txt"
    (time . putStrLn . Prelude.last . Prelude.lines) fileContent
    fileContent <- BS.readFile "./bible.txt"
    (time . BC.putStrLn . Prelude.last . BC.lines) fileContent
    fileContent <- TIO.readFile "./bible.txt"
    (time . TIO.putStrLn . Prelude.last . T.lines) fileContent
