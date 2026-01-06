{-# LANGUAGE DeriveGeneric #-} -- This pragma is for deriving JSON structures for custom types
{-# LANGUAGE DeriveAnyClass #-} -- This pragma is for deriving JSON structures for custom types
{-# LANGUAGE DuplicateRecordFields #-} -- This pragma is nessecary for question 5, which has duplicate fields in different record types.
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import GHC.Generics
import Data.Maybe
import Data.ByteString.Lazy as LBS
import Network.HTTP.Conduit

-- Question 1
-- Add the correct instances that should be derived in the below data types for deriving JSON formats for them.

data Continent = Africa | Asia | Europe | NorthAmerica | SouthAmerica | Oceania | Antartica deriving (Show)

data Country = Country { name :: String, population :: Int, location :: Continent } deriving (Show)

-- Question 2
-- Decode these countries to the type Country above.

jsonItaly :: LBS.ByteString
jsonItaly = "{\"location\":\"Europe\",\"name\":\"Italy\",\"population\":55550000}"

jsonSpain :: LBS.ByteString
jsonSpain = "{\"location\":\"Europe\",\"name\":\"Spain\",\"population\":47350000}"

jsonAustralia :: LBS.ByteString
jsonAustralia  = "{\"location\":\"Oceania\",\"name\":\"Australia\",\"population\":25690000}"

jsonIndia :: LBS.ByteString
jsonIndia = "{\"location\":\"Asia\",\"name\":\"India\",\"population\":1380000000}"

-- Question 3
-- Add the correct instances that should be derived in the below data type for deriving JSON formats.

data Tree = Leaf | LLeaf Tree | RLeaf Tree deriving (Show)

-- Question 4
-- Encode the below example of a Tree to JSON and try to figure out how it is structured.

exampleLeaf = LLeaf $ RLeaf $ RLeaf $ LLeaf $ Leaf

-- Question 4
-- With the below getUser function we can fetch some API that will serve us a user from a list of 10 users.
-- Write a Haskell data type that can convert this JSON representation of a user to a Haskell datatype

{- The following is JSON structured and is the output of the API for user 4 out of the 10.

{
  "id": 4,
  "name": "Patricia Lebsack",
  "username": "Karianne",
  "email": "Julianne.OConner@kory.org",
  "address": {
    "street": "Hoeger Mall",
    "suite": "Apt. 692",
    "city": "South Elvis",
    "zipcode": "53919-4257",
    "geo": {
      "lat": "29.4572",
      "lng": "-164.2990"
    }
  },
  "phone": "493-170-9623 x156",
  "website": "kale.biz",
  "company": {
    "name": "Robel-Corkery",
    "catchPhrase": "Multi-tiered zero tolerance productivity",
    "bs": "transition cutting-edge web services"
  }
}

-}

getUserFromAPI :: Int -> IO LBS.ByteString
getUserFromAPI n = do if n `Prelude.elem` [1..10] 
                      then do json <- simpleHttp $ "https://jsonplaceholder.typicode.com/users/" ++ (show n) 
                              return json
                      else return "given integer is not in the range [1..10]"

-- Question 5
-- Replace this main function to get an user number between [1..10] and return return the parsed User from the api.

main :: IO ()
main = do Prelude.putStrLn "Fix me :)"
