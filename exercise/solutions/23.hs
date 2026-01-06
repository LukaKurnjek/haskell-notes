
import System.Random (mkStdGen, Random(randomR), StdGen)
import Data.Map as Map ( fromList, insert, lookup, Map )
import Control.Monad.State ( evalState, runState, MonadState(put, get), State )
import Data.Maybe ( fromJust )
import Text.Read (readMaybe)

{-
Question 1
Write a program that simulates the Tic-Tac-Toe game https://en.wikipedia.org/wiki/Tic-tac-toe. 
Instead of two players playing against each other use the System.Random module to randomly 
pick the X and O choices. You do not have to check if any of the players has won the game.
Simply print the board results at the end of the game. Use a State monad to implement the code.

Developer comment (remove after review):
The case when 2 actual players play against each other and check who has won will be a homework
in the next lesson, because you can nicely code this with help of monadic functions.
-}

data Player = XPlayer | OPlayer deriving Eq
data Choice = Empty | X | O deriving Eq

data GameState = GameState
  { currentBoard :: [Choice]
  , currentPlayer :: Player
  , generator :: StdGen
  }

main1 :: IO ()
main1 = do
  let gen = mkStdGen 1
      initState = GameState
                  [Empty | boardInd <- [1..9]]
                  XPlayer
                  gen
  playGame initState

playGame :: GameState -> IO ()
playGame gs = do
  let (gameFinished, newGS) = runState resolveTurn gs
  if gameFinished 
  then do
    putStrLn "Game results:"
    printBoard newGS
  else playGame newGS

resolveTurn :: State GameState Bool
resolveTurn = do
  choice <- chooseRandomMove
  applyMove choice
  isGameDone

chooseRandomMove :: State GameState Int
chooseRandomMove = do
  gs <- get
  let board = currentBoard gs
      openSpots = [ind | ind <- [0..8], board !! ind == Empty]
      gen = generator gs
  let (i, gen') = randomR (0, length openSpots - 1) gen
  put $ gs { generator = gen' }
  return $ (openSpots !! i) + 1

applyMove :: Int -> State GameState ()
applyMove choice = do
  gs <- get
  let player = currentPlayer gs
      board = currentBoard gs
      newBoard = if player == XPlayer
                 then [if ind /= choice then board !! (ind-1) else X | ind <- [1..9]]
                 else [if ind /= choice then board !! (ind-1) else O | ind <- [1..9]]
  put $ gs { currentPlayer = nextPlayer player, currentBoard = newBoard }

nextPlayer :: Player -> Player
nextPlayer XPlayer = OPlayer
nextPlayer OPlayer = XPlayer

isGameDone :: State GameState Bool
isGameDone = do
  gs <- get
  let board = currentBoard gs
      openSpots = [ind | ind <- [0..8], board !! ind == Empty]
  return $ length openSpots == 0

printBoard :: GameState -> IO ()
printBoard gs = do
    let board = currentBoard gs
    let stateToString st = case st of
                             Empty -> "-"
                             X -> "X"
                             O -> "O"
        printInd ind = stateToString $ board !! ind
    mapPutStr [printInd 0,"|", printInd 1,"|", printInd 2, "\n"]
    putStrLn "-----"
    mapPutStr [printInd 3,"|", printInd 4,"|", printInd 5, "\n"]
    putStrLn "-----"
    mapPutStr [printInd 6,"|", printInd 7,"|", printInd 8, "\n"]

mapPutStr :: [String] -> IO ()
mapPutStr [] = return ()
mapPutStr [x] = putStr x
mapPutStr (x:xs) = putStr x >> mapPutStr xs

{-
Question 2
Write a program for creating a shopping list, where the user can add 3 kinds of
flowers. When the program is started it should display the following message:
    Possible flowers are: daisy, sunflower and tulip.
    Possible actions are:
        add  --flower --amount 
        remove --flower --amout 
        show_list 
If the user does not use a valid comamnd and appropriate options notify him about
this. When the user says show_list print a message of how many of which flowers are
added to the list. Use a state monad when coding the solution.
-}

type Basket = Map.Map String Int
type Command = String
type Options = [String]
type ShoppingData = (Basket, Command, Options)
type UserMessage = String

main2 :: IO ()
main2 = do
    putStrLn "Possible flowers are: daisy, sunflower and tulip."
    putStrLn "Possible actions are:\n \
            \ add  --flower --amount \n \
            \ remove --flower --amout \n \
            \ show_list \n"

    let emptyBasket = Map.fromList [("daisy", 0),("sunflower",0),("tulip",0)]
    shop emptyBasket

shop :: Basket -> IO ()
shop basket = do
    putStrLn "What would you like to do:"
    fullCommand <- getLine

    if Prelude.null fullCommand
    then do
        putStrLn "No command was given."
        shop basket
    else do
        let (command:options) = words fullCommand
        case command of
            "add" -> do
                let (msg, (newBasket,_,_)) = runState updateOrder (basket, command, options)
                putStrLn msg
                shop newBasket
            "remove" -> do
                let (msg, (newBasket,_,_)) = runState updateOrder (basket, command, options)
                putStrLn msg
                shop newBasket
            "show_list" -> do
                print $ evalState updateOrder (basket, command, options)
                shop basket
            _ -> do
                putStrLn "Not a valid command."
                shop basket

updateOrder :: State ShoppingData UserMessage
updateOrder = do
    (basket, command, options) <- get
    if command == "show_list"
    then return $ describeBasket basket
    else do
        if length options /= 2
        then return "Options for this command should equal to 2."
        else do
            let flower = options !! 0
                amount = readMaybe (options !! 1) :: Maybe Int
            if flower `notElem` ["daisy","sunflower","tulip"] || amount == Nothing
            then return "The options for this command are not correct."
            else do
                let changeAmount = fromJust amount
                    currentAmount = fromJust $ Map.lookup flower basket
                case command of
                    "add" -> do
                        let updatedbasket = Map.insert flower (changeAmount + currentAmount) basket
                        put (updatedbasket, command, options)
                        return "Updated basket."
                    "remove" -> do
                        let updatedAmount = currentAmount - changeAmount
                        if updatedAmount < 0
                        then do
                            let updatedbasket = Map.insert flower 0 basket
                            put (updatedbasket, command, options)
                            return "Updated basket."
                        else do
                            let updatedbasket = Map.insert flower updatedAmount basket
                            put (updatedbasket, command, options)
                            return "Updated basket."
                    _ -> return "This case will anyway never happen"

describeBasket :: Basket -> UserMessage
describeBasket basket = "The shopping list contains " ++ 
                        show daisy ++ " daisies, " ++
                        show sunflower ++ " sunflowers and " ++
                        show tulip ++ " tulips."
  where daisy = fromJust $ Map.lookup "daisy" basket
        sunflower = fromJust $ Map.lookup "sunflower" basket
        tulip = fromJust $ Map.lookup "tulip" basket
        