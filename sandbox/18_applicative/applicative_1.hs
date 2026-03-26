import Data.Maybe ( fromJust )

maybeAddition :: Num a => Maybe a -> Maybe (a -> a)
maybeAddition maybeVal = (+) <$> maybeVal

-- Here is a more detailed breakdown of this function without using <$>
maybeAddition' :: Num a => Maybe a -> Maybe (a -> a)
maybeAddition' Nothing = Nothing
maybeAddition' (Just n) = Just $ (+) n

var1 :: Maybe Int
var1 = Just 1

maybeAdd1 :: Maybe (Int -> Int)
maybeAdd1 = maybeAddition var1

var2 :: Maybe Int
var2 = Just 2

add1 :: Maybe Int -> Maybe Int
add1 Nothing = Nothing
add1 (Just n) = Just $ fromJust maybeAdd1 $ n

list1 = [1,2,3] :: [Int]
list2 = [4,5] :: [Int]

allCombinations :: Num a => [a] -> [a] -> [a]
allCombinations l1 l2 = pure (+) <*> l1 <*> l2

main :: IO ()
main = do
  print $ add1 var2
  print $ allCombinations list1 list2