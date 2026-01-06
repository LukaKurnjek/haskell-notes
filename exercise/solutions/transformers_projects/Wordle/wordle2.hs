
{- Wordle game

A player has to guess the name of an animal. He is beeing told how many
characters the name contains. For every guess colors shown for each letter
indicate if the letter is correct (green), if it exist in another place (yellow) 
or if it does not exist at all (white). 
  
The player has 6 attempts to guess the word. After that a new word is randomly
chosen from the animals.txt file contained in the same folder.

In addition to version 1 this version also saves the result to a file. If the
game is interrupted and later resumed the intermediat results are loaded from
the file and the player can continue the game where he left it. 
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

import qualified Data.Text as T 
import qualified Data.Text.IO as TIO
import Text.Printf (printf)
import Control.Monad (join)
import System.Random (mkStdGen, Random(randomR), StdGen)
import Data.Aeson (decode, encode, FromJSON, ToJSON)
import Data.ByteString.Lazy as B (length, readFile, writeFile)
import Data.ByteString.Lazy.Char8 as BC (ByteString, pack, unpack)
import GHC.Generics (Generic)
import Data.Maybe (fromJust, isNothing)
import System.Directory (doesFileExist, removeFile)

-- List of animals for the game
type Animals = [T.Text]
-- Generator for the random function
type Generator = StdGen

-- Possible states for a letter guess
data State = Fail | Success | Misplace

-- The game state type used for reading or writing to JSON.
data GameState = GameState
  { currentAnimal :: T.Text
  , currentGuesses :: [T.Text]
  } deriving (Show, Generic, ToJSON, FromJSON)

-- Main function that starts the game
main :: IO ()
main = do
    Prelude.putStrLn $ "Game rules:\n" ++ 
            "The game let's you know the word lenght.\n" ++
            "You can type in any characters and as many as you want.\n" ++
            "If you provide to much characters the surplus will be discarted."
    let filename = "animals.txt"
    animalsText <- Prelude.readFile filename
    let animals =
            join
            . map T.words
            . T.lines
            . T.toLower
            $ T.pack animalsText
        gen = mkStdGen 1

    -- Checks existing game data and passes the data onto the play function
    jsonData <- returnGameData
    let gameData = if B.length jsonData > 0
                   then (decode jsonData :: Maybe GameState)
                   else (Nothing :: Maybe GameState)
    if isNothing gameData
    then startGame animals gen
    else do
        Prelude.putStrLn "\nLoading saved game."
        let initState = fromJust gameData
            animal = currentAnimal initState
            wordles = currentGuesses initState

        Prelude.putStrLn $ "The lenght of the word is: " ++ show (T.length animal)
        wordles <- play animal (6 - Prelude.length wordles) wordles
        Prelude.putStrLn "Your guesses:"
        TIO.putStrLn . T.unlines . reverse $ wordles
        startGame animals (mkStdGen 2)

-- Retuns the existing game data in form of a Bytestring
returnGameData :: IO ByteString
returnGameData = do
  fileExists <- doesFileExist "./game_data"
  if fileExists
  then do
    B.readFile "./game_data"
  else do
    let jsonData = BC.pack ""
    return jsonData

-- Randomaly picks a word, starts the game and then repeats.
startGame :: Animals -> Generator -> IO ()
startGame animals gen = do
  let (selected_index, gen') = randomR (0, Prelude.length animals - 1) gen
      selected_word = animals !! selected_index
  
  Prelude.putStrLn $ "The lenght of the word is: " ++ show (T.length selected_word)

  wordles <- play selected_word 6 []
  Prelude.putStrLn "Your guesses:"
  TIO.putStrLn . T.unlines . reverse $ wordles
  startGame animals gen'

-- Shows a color indicator for a given state
cshow :: State -> Char
cshow = \case
  Fail -> '⬜'
  Success -> '🟩'
  Misplace -> '🟨'

-- Processes one turn of a game. Takes in the seeked word,
-- asks the user for a guess and prints back the colored text
-- that gives the user informations about his guess. 
play :: T.Text -> Int -> [T.Text] -> IO [T.Text]
play animal attempts list = go attempts list
  where
    go :: Int -> [T.Text] -> IO [T.Text]
    go 0 xs = return xs
    go n xs = do
      let i = 1 + Prelude.length xs

      Prelude.putStrLn $ "Please enter your animal " ++ show i ++ "/6: "

      -- Gets input from the user and processes it
      attemptstr <- getLine
      let attemp = T.toLower . T.strip $ T.pack attemptstr
          (wordle, correct) = getWordle attemp animal
          wordles = wordle : xs

      printf "Current attempt: %d/6\n\n" i
      TIO.putStrLn wordle

      -- Checks if the game is finished. If not it updates 
      -- the game state and moves to the next step.
      if correct
      then do
        Prelude.putStrLn "Congratulation!"
        printf "Attempts: %d/%d\n\n" i attempts
        removeFile "./game_data"
        return wordles
      else do
        let gs = GameState
                  { currentAnimal = animal
                  , currentGuesses = wordles
                  }
            encodedJson = encode gs
        B.writeFile "./game_data" encodedJson
        go (n - 1) wordles

-- Takes in the actual and guessed word. Retuns a tuple where the first
-- element is the colored text and the second tells if the guess was correct.
getWordle :: T.Text -> T.Text -> (T.Text, Bool)
getWordle attempt correct =
  let result = T.zipWith go attempt correct
      rest = if T.length attempt < T.length correct
             then T.pack $ replicate (T.length correct - T.length attempt) $ cshow Fail
             else mempty
      isCorrect = attempt == correct
   in (result <> rest, isCorrect)
  where
    go :: Char -> Char -> Char
    go ca cc
      | ca == cc = cshow Success
      | ca `elem` T.unpack correct = cshow Misplace
      | otherwise = cshow Fail 
