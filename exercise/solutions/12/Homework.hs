
-- Question 1
-- Write a function that takes in an integer n and returns a list of the 
-- first n fibonacci numbers. Use the function scanl to achieve this.

fibs :: Int -> [Int]
fibs n = let fib = 1 : scanl (+) 1 fib 
         in take n fib

test :: IO ()
test = print $ fibs 7

-- Question 2
-- In the previous homework you saw 3 functions how to generate prime numbers
-- that you could not understand since we have not covered the code syntax the
-- are using yet. Write now a such a function with the knowledge you have. 

-- Create a cabal project with an app/ folder that contains Main.hs and a lib/
-- folder that contains Libraries.hs. In the Libraries.hs file specify a function
-- that takes an integer and returns True if it is prime and False if it is not.

-- Then in Main.hs let the program ask the user for a number and let him know
-- wheather it is prime or not by using the module defined in Libraries.hs.

-- @Robertino, Thomas:
-- I thought the first example that uses cabal should be without adding any 
-- external packages to the dependency list, so they can first earn the basic 
-- procedure of setting up a cabal project. 

-- Question 3
-- Create a cabal project where the main file contains a program that asks the 
-- user to input two numbers in scientific format and sums them. The library
-- file should contain a code that checks if the input number has the correct 
-- scientific format. If one or both of the numbers do not have the correct
-- format notify the user about it and tell him he should try again.  

-- @Robertino, Thomas:
-- I could not figure out any other package that is not included in the standard
-- Haskell instalation and is simple enough for the student to use at this point 
-- in the course. If you have any idea feel free to change this example. 
-- I think the added value of this example is that the student has to figure out  
-- that the scientific package needs to be added as a dependency to both library 
-- and executable sections in the cabal file. 