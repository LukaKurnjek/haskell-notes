
{- Wordle game

A player has to guess the name of an animal. He is beeing told how many
characters the name contains. For every guess colors shown for each letter
indicate if the letter is correct (green), if it exist in another place (yellow) 
or if it does not exist at all (white). 
  
The player has 6 attempts to guess the word. After that a new word is randomly
chosen from the animals.txt file contained in the same folder.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
  ( Text,
    pack,
    strip,
    toLower,
    unpack
  )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf (printf)
import Control.Monad (join)
import System.Random (mkStdGen, Random(randomR), StdGen)

-- List of animals for the game
type Animals = [T.Text]
-- Generator for the random function
type Generator = StdGen

-- Possible states for a letter guess
data State = Fail | Success | Misplace

-- Main function that starts the game
main :: IO ()
main = do
    let filename = "animals.txt"
    animalsText <- readFile filename
    let animals =
            join
            . map T.words
            . T.lines
            . toLower
            $ pack animalsText
        gen = mkStdGen 1

    putStrLn $ "Game rules:\n" ++ 
               "The game let's you know the word lenght.\n" ++
               "You can type in any characters and as many as you want.\n" ++
               "If you provide to much characters the surplus will be discarted."
    startGame animals gen

-- Randomaly picks a word, starts the game and then repeats.
startGame :: Animals -> Generator -> IO ()
startGame animals gen = do
  let (selected_index, gen') = randomR (0, length animals - 1) gen
      selected_word = animals !! selected_index
  
  putStrLn $ "The lenght of the word is: " ++ show (T.length selected_word)

  wordles <- play selected_word
  TIO.putStrLn . T.unlines . reverse $ wordles
  startGame animals gen'

-- Shows a color indicator for a given state
cshow :: State -> Char
cshow = \case
  Fail -> '⬜'
  Success -> '🟩'
  Misplace -> '🟨'

-- Number of attemps a user has to guess a word
attempts :: Int
attempts = 6

-- Processes one turn of a game. Takes in the seeked word,
-- asks the user for a guess and prints back the colored text
-- that gives the user informations about his guess. 
play :: Text -> IO [Text]
play selected_word = go attempts []
  where
    go :: Int -> [Text] -> IO [Text]
    go 0 xs = return xs
    go n xs = do
      let i = 1 + length xs

      putStrLn $ "Please enter your animal " ++ show i ++ "/" ++ show attempts ++ ": "

      -- Gets input from the user and processes it
      attemptstr <- getLine
      let attemp = toLower . strip $ pack attemptstr
      let (wordle, correct) = getWordle attemp selected_word

      printf "Current attempt: %d/%d\n\n" i attempts
      TIO.putStrLn wordle

      -- Checks if the game is finished. If not it moves to the next step.
      if correct
        then do
          putStrLn "Congratulation!"
          printf "Attempts: %d/%d\n\n" i attempts
          return (wordle : xs)
        else do
          go (n - 1) (wordle : xs)

-- Takes in the actual and guessed word. Retuns a tuple where the first
-- element is the colored text and the second tells if the guess was correct.
getWordle :: Text -> Text -> (Text, Bool)
getWordle attempt correct =
  let result = T.zipWith go attempt correct
      rest =
        if T.length attempt < T.length correct
          then pack $ replicate (T.length correct - T.length attempt) $ cshow Fail
          else mempty
      isCorrect = attempt == correct
   in (result <> rest, isCorrect)
  where
    go :: Char -> Char -> Char
    go ca cc
      | ca == cc = cshow Success
      | ca `elem` unpack correct = cshow Misplace
      | otherwise = cshow Fail
