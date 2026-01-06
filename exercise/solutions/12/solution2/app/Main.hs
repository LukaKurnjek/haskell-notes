module Main where

import Libraries (isPrime)

main :: IO ()
main = do
  print "Input a number:"
  n <- getLine
  let primality = isPrime $ read n
  if primality
    then print "Number is prime."
    else print "Number is not prime."
