{-

**************************** IMPORTANT ****************************

Solve this homework after completing and checking the "Maze" one.

*******************************************************************

We're going to build on top of the "Maze" challenge by coding a similar
but a bit more complicated game.

It works the same as the "Maze" game, with the difference that the player
is now in a forest. Because we're in a forest, there are no walls. And,
if you walk long enough, you're guaranteed to find the exit.

So, what's the challenge in playing this game? The challenge lies in that
now we have "stamina." Stamina is a number (we start with 10). And, each
time the player makes a move, its stamina gets reduced by the amount of work
needed to cross the current trail (represented by a number contained in the
value constructor).

The data types and functions are pretty much the same, with a few caveats:

- We don't have walls.
- We don't want to choose a specific numeric type, but we want to make sure
we can do basic numeric operations regardless of the type we pass to the functions.
- Because now we have to keep track of the player's stamina, we'll need to
move it around with our current forest. This would be an awesome use case
for monads, but because we don't know how to use them yet, a "(stamina, forest)"
pair will have to do.

Using GHCi, like the "Maze" game, this game should look like this:

*Main> solveForest testForest []
"You have 10 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward ]
"You have 7 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward]
"You have 4 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward, GoLeft  ]
"You ran out of stamina and died -.-!"
*Main> solveForest testForest [GoForward, GoLeft , GoRight]
"YOU'VE FOUND THE EXIT!!"
-}

-- Step 1
data Move = GoForward | GoLeft | GoRight

data Forest a = FoundExit | Trail a (Forest a) (Forest a) (Forest a) deriving (Show)

-- Step 2
move :: Num a => (a, Forest a) -> Move -> (a, Forest a)
move (s, FoundExit) _ = (s, FoundExit)
move (s, Trail a x _ _) GoLeft = (s - a, x)
move (s, Trail a _ x _) GoForward = (s - a, x)
move (s, Trail a _ _ x) GoRight = (s - a, x)

-- Step 3
testForest :: Forest Int
testForest =
  Trail
    3
    ( Trail
        7
        (Trail 3 FoundExit FoundExit FoundExit)
        (Trail 4 FoundExit FoundExit FoundExit)
        (Trail 5 FoundExit FoundExit FoundExit)
    )
    ( Trail
        3
        (Trail 3 FoundExit FoundExit FoundExit)
        (Trail 9 FoundExit FoundExit FoundExit)
        (Trail 5 FoundExit FoundExit FoundExit)
    )
    ( Trail
        5
        (Trail 3 FoundExit FoundExit FoundExit)
        (Trail 4 FoundExit FoundExit FoundExit)
        (Trail 1 FoundExit FoundExit FoundExit)
    )

-- Step 4
solveForest' :: (Show a, Ord a, Num a) => Forest a -> [Move] -> (a, Forest a)
solveForest' forest = foldl move (10, forest)

-- Step 5
showCurrentChoice :: (Show a, Ord a, Num a) => (a, Forest a) -> String
showCurrentChoice (s, f)
  | s <= 0 = "You ran out of stamina and died -.-!"
  | otherwise = case f of
    FoundExit -> "YOU'VE FOUND THE EXIT!!"
    _ -> "You have " ++ show s ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

-- Step 6
solveForest :: (Show a, Ord a, Num a) => Forest a -> [Move] -> String
solveForest forest moves = showCurrentChoice $ foldl move (10, forest) moves
