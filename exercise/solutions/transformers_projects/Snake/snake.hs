
{- Snake game

You can check how the game is played here:
http://www.papg.com/show?3AE4

In our game two players compete against each other
instead of a player competing against a computer.
-}

import Control.Monad (forM_, when, unless)
import System.Random (mkStdGen, Random(randomR), StdGen)
import Control.Monad.State (MonadState(get, put), runState, evalState, State)
import System.Console.ANSI (clearScreen)

-- Valid moves are: a (left), d (right), w (up), s (down)
type Move = Char
type ValidMove = Bool
type GameFinished = Bool
-- Board indexes goe from 1 to 25 (it's a 5x5 board)
-- starting in the upper left corner and ending in the lower right corner
type BoardIndex = Int
-- A connection implies the trail a snake has left that goes from one filed to the other
type Connection = (BoardIndex,BoardIndex)

-- There are 2 players 
data Player = Player1 | Player2 deriving (Show, Eq)
-- State type for the state monad
data GameState = GameState
  { playerPositions :: [BoardIndex] -- 1st element is Player1 position, 2nd is Player2 position
  , connectionsP1 :: [Connection] -- defines the trails that the snake of Player1 leaves
  , connectionsP2 :: [Connection] -- defines the trails that the snake of Player2 leaves
  , currentPlayer :: Player -- current player on the move
  }

-- Main function of the game
main :: IO ()
main = do
    let gen = mkStdGen 1
    playGame True (initialGameState gen)

-- Initial game state that picks two radnom positions for Player1 and Player2
initialGameState :: StdGen -> GameState
initialGameState gen = GameState
    [p1Pos, p2Pos]
    []
    []
    Player1
  where (p1Pos, gen1) = randomR (1, 25) gen
        (p2Pos, gen2) = randomR (1, 25) gen1

-- Function that triggers the actions for a turn and decides if to continue the game
playGame :: ValidMove -> GameState -> IO ()
playGame moveValid gs = do
    clearScreen
    putStrLn "\nGame options: \n\
             \Up/Down/Left/right is done with keys w/s/a/d.\n"

    putStrLn "Current board is:"
    printBoard gs
    unless moveValid $ do
        putStrLn "Move not valid. Try again:"

    let player = currentPlayer gs
    if evalState isGameFinished gs
    then do
        putStrLn "Game is finished."
        putStrLn $ show (nextPlayer player) ++ " won the game:"
    else do
        putStrLn $ show player ++ " make you move:"
        move <- getMove

        let (moveValid, newGs) = runState (processMove move) gs
        if moveValid
        then playGame moveValid newGs
        else playGame moveValid gs

-- State monad that checks weather the current player can move in any possible direction
isGameFinished :: State GameState GameFinished
isGameFinished = do
    gs <- get
    let (canMoveRight, _) = runState (processMove 'd') gs
        (canMoveLeft, _) = runState (processMove 'a') gs
        (canMoveUp, _) = runState (processMove 'w') gs
        (canMoveDown, _) = runState (processMove 's') gs
        moves = [canMoveRight, canMoveLeft, canMoveUp, canMoveDown]
    return $ all not moves

-- State monad function that processes a move and updates the state accordingly
-- It return a Bool that says weather the move is valid or not
processMove :: Move -> State GameState ValidMove
processMove move = do
    gs <- get 

    -- Helper functions and variables
    let moveFunction = case move of
                            'a' -> flip (-) 1
                            'd' -> (+1)
                            'w' -> flip (-) 5
                            's' -> (+5)
                            _ -> id
        isPlayer1 = currentPlayer gs == Player1
        currentPos = if isPlayer1
                     then playerPositions gs !! 0
                     else playerPositions gs !! 1
        newPosition = moveFunction currentPos
        connections = connectionsP1 gs ++ connectionsP2 gs
    
    -- Validity checks for current move 
    let freeFieldCheck = newPosition `elem` getFreeFields connections
        boardLimitsCheck = case move of
                            'a' -> currentPos `notElem` [1,6,11,16,21]
                            'd' -> currentPos `notElem` [5,10,15,20,25]
                            'w' -> currentPos `notElem` [1,2,3,4,5]
                            's' -> currentPos `notElem` [21,22,23,24,25]
                            _ -> False
        validMove = freeFieldCheck && boardLimitsCheck 
    
    -- Updating game state if move is valid
    when validMove $ do 
        let updatedPositions = if isPlayer1
                               then [newPosition, playerPositions gs !! 1]
                               else [playerPositions gs !! 0, newPosition]
            (newConnectionsP1,newConnectionsP2) = if isPlayer1
                                                then (connectionsP1 gs ++ [sortIntTuple (currentPos, newPosition)], connectionsP2 gs)
                                                else (connectionsP1 gs, connectionsP2 gs ++ [sortIntTuple (currentPos, newPosition)])
            newGs = gs { playerPositions = updatedPositions
                        , connectionsP1 = newConnectionsP1
                        , connectionsP2 = newConnectionsP2
                        , currentPlayer = nextPlayer $ currentPlayer gs} 
        put newGs
    
    return validMove 

-- Function that returns all board indexes that are not occupied yet
getFreeFields :: [Connection] -> [BoardIndex]
getFreeFields connections = filter (`notElem` occupiedFields) [1..25]
    where tuplesToList [] = []
          tuplesToList ((x1,x2):xs) = x1 : x2 : tuplesToList xs
          occupiedFields = tuplesToList connections

-- Helper function for sorting a tuple of two. Is needed because the printBoard 
-- functions expects that connections of board indexes are ordered tuples.
sortIntTuple :: (Int, Int) -> (Int, Int)
sortIntTuple (x1, x2) = if x1 > x2 
                        then (x2, x1)
                        else (x1, x2)

-- Function that takes in the current player and returns the next player
nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

-- Function that prints the game board depending on the current game state 
printBoard :: GameState -> IO ()
printBoard gs = do
    let connections = connectionsP1 gs ++ connectionsP2 gs
        [pos1,pos2] = playerPositions gs

    let verticalLines n = [char ++ " " | pos <- [1..5], 
                                         let relPos = pos + n*5
                                             char = if (relPos, pos + (n + 1)*5) `elem` connections 
                                                    then "|" 
                                                    else " "
                          ]
        dots n = [char | relPos <- [1..5], 
                         let pos = relPos + n*5
                             char = if pos `elem` [pos1,pos2] 
                                    then (if pos == pos1 
                                          then "1" 
                                          else "2") 
                                    else "*"
                 ]
        horizontalLines n = [char | pos <- [1..4], 
                                    let relPos = pos + n*5
                                        char = if (relPos, relPos + 1) `elem` connections 
                                               then "─" 
                                               else " "
                            ]
        horizontalConnections n = merge (dots n) (horizontalLines n)
    
    forM_ [0..3] $ \i -> do
        putStrLn $ mconcat $ horizontalConnections i
        putStrLn $ mconcat $ verticalLines i 
        when (i == 3) $ do
            putStrLn $ mconcat $ horizontalConnections 4
    
    where merge xs [] = xs
          merge [] ys = ys
          merge (x:xs) ys = x:merge ys xs

-- IO action that askes the user to input a move and checks if it is valid 
getMove :: IO Move
getMove = do
    move <- getLine
    if length move == 1
    then do
        let charMove = head move
        if charMove `elem` "asdw"
        then return charMove
        else do
            putStrLn "Not a valid move. Try again."
            getMove
    else do
        putStrLn "You have to input only one character."
        getMove
