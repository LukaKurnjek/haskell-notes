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

data Continent = Africa | Asia | Europe | NorthAmerica | SouthAmerica | Oceania | Antartica deriving (Show, Generic, ToJSON, FromJSON)

data Country = Country { name :: String, population :: Int, location :: Continent } deriving (Show, Generic, ToJSON, FromJSON)

-- Question 2
-- Decode these countries to the type Country above.

jsonItaly :: LBS.ByteString
jsonItaly = "{\"location\":\"Europe\",\"name\":\"Italy\",\"population\":55550000}"

decodedItaly = decode jsonItaly :: Maybe Country

jsonSpain :: LBS.ByteString
jsonSpain = "{\"location\":\"Europe\",\"name\":\"Spain\",\"population\":47350000}"

decodedSpain = decode jsonSpain :: Maybe Country

jsonAustralia :: LBS.ByteString
jsonAustralia  = "{\"location\":\"Oceania\",\"name\":\"Australia\",\"population\":25690000}"

decodedAustralia = decode jsonAustralia :: Maybe Country

jsonIndia :: LBS.ByteString
jsonIndia = "{\"location\":\"Asia\",\"name\":\"India\",\"population\":1380000000}"

decodedIndia = decode jsonIndia :: Maybe Country

-- Question 3
-- Add the correct instances that should be derived in the below data type for deriving JSON formats.

data Tree = Leaf | LLeaf Tree | RLeaf Tree deriving (Show, Generic, ToJSON, FromJSON)

-- Question 4
-- Encode the below example of a Tree to JSON and try to figure out how it is structured.

exampleLeaf = LLeaf $ RLeaf $ RLeaf $ LLeaf $ Leaf

encodedExampleLeaf = encode exampleLeaf

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

data Geo = Geo {
          lat :: String
        , lng :: String
} deriving (Show, Generic, ToJSON, FromJSON)

data Address = Address {
          street :: String
        , suite :: String
        , city :: String 
        , zipcode :: String
        , geo :: Geo
} deriving (Show, Generic, ToJSON, FromJSON)

data Company = Company {
          name :: String
        , catchPhrase :: String
        , bs :: String
} deriving (Show, Generic, ToJSON, FromJSON)

data User = User {
          id :: Int
        , name :: String
        , username :: String
        , email :: String
        , address :: Address
        , phone :: String
        , website :: String
        , company :: Company
} deriving (Show, Generic, ToJSON, FromJSON)

getUserAsData :: Int -> IO (Maybe User)
getUserAsData n = do bs <- getUserFromAPI n
                     let user = decode bs :: Maybe User
                     return user

-- Question 5
-- Replace this main function to get an user number between [1..10] and return return the parsed User from the api.

main :: IO ()
main = do Prelude.putStrLn "Enter a number from [1..10]"
          input <- getLine
          let n = read input :: Int
          if n `Prelude.elem` [1..10]
             then 
                do mUser <- getUserAsData n
                   let user = fromJust mUser
                   Prelude.putStrLn $ show user 
             else Prelude.putStrLn "number not in range"
