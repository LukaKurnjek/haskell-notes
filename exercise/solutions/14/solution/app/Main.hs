
module Main where

import Data.List.Split (splitOn)
import Data.Char (isDigit)

data Path a b = Success a | Failure b
type StringPath = Path [String] String

instance Show b => Show (Path a b) where
  show (Success _) = "The data has passed all the checks."
  show (Failure msg) = show msg

-- The main function of the programm
main :: IO ()
main = do
    let fileName = "./app/Data.txt"
    input <- readFile fileName

    let [_, name, email, password] = getInputData input
    let userData = Success [name, email, password] :: Path [String] String
    let result = checkPassword . checkEmail . checkName $ userData
    print result

-- Helper functions
getInputData :: String -> [String]
getInputData inputString = do
      let fileLines = lines inputString
      map (\el -> snd $ splitAt 10 el) fileLines

process :: StringPath -> (String -> StringPath) -> Int -> StringPath
process userData checkFunction index = 
  case userData of
      Success xs -> checkFunction $ xs !! index
      Failure msg -> Failure msg

-- Functions that make a certain check for the user data
checkName :: StringPath -> StringPath
checkName userData = process userData check 0
  where check input = if length (words input) > 1
                      then userData
                      else Failure "Name has to contain at least 2 parts."

checkEmail :: StringPath -> StringPath
checkEmail userData = process userData check 1
  where check input = if '@' `elem` input && '.' `elem` secondPart input
                      then userData
                      else Failure "Email has to contain @ and a dot in the domain name."
        secondPart input = splitOn "@" input !! 1

checkPassword :: StringPath -> StringPath
checkPassword userData = process userData check 2
  where check input = if length input > 5 && any isDigit input && (not $ all isDigit input)
                      then userData
                      else Failure "Password has less than 6 characters or does not contain at least one number and one other character."