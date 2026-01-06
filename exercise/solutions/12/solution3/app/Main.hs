module Main where

import Libraries (parseNumber)

import Data.Scientific (Scientific)

main :: IO ()
main = do
    print "Input first number:"
    n1 <- getLine
    print "Input second number:"
    n2 <- getLine
    let (validity1, n1Sci) = parseNumber n1
        (validity2, n2Sci) = parseNumber n2
    if validity1 && validity2
    then do
        let sumSci = n1Sci + n2Sci
        putStrLn $ "Sum is: " ++ show sumSci
    else do
        putStrLn "Not a valid input for a scientific number. Try again."
        main
