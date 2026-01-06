
-- Question 1
-- Write a function that takes in an aritmetic operator that has an instance of Fractional as +, -, 
-- * or /. It also takes in a list of type [Double]. Then it calulates the number of all possible 
-- computations where you can take any of two elements from the list and uses the provided operator 
-- on them. For which of the stated operators above is the number the smallest for the list [1..5]? 

uniqueCombinations :: (Fractional a, Eq a) => (a -> a -> a) -> [a] -> Int
uniqueCombinations func myList = length $ unique allCombinations
    where allCombinations = map func myList <*> myList
          unique [] = []
          unique (x:xs) = x : unique (filter (/= x) xs)

printCombs :: (Fractional a, Eq a) => [a -> a -> a] -> [a] -> IO ()
printCombs operators list = go 0
  where go ind = do
                  if ind < length operators
                  then do
                      let operator = operators !! ind
                      print $ uniqueCombinations operator list
                      go (ind + 1)
                  else 
                      putStrLn "Finished."

myOperators :: Fractional a => [a -> a -> a]
myOperators = [(+), (-), (*), (/)]

main1 :: IO ()
main1 = do
    printCombs myOperators [1..5]

-- Question 2
-- For the Cube type and data defined below create a Show instance that prints possible combinations
-- of the numbers and their probabilites. Create a Num Semigroup instance that combines e.g. the strings
-- "1" and "2" to the string "1-2". Create a Semigroup and Monoid instance for Cube that combines all
-- posible cube results for 2 cubes and their probabilities into a new Cube object. Then evalueate:
-- cube1 <> cube2 and mconcat [cube1, cube1, cube1]. The result for cube1 <> cube2 should be:

-- Case: 1-1, Probability: 6.0e-2
-- Case: 1-2, Probability: 9.0e-2
-- Case: 1-3, Probability: 0.15
-- Case: 2-1, Probability: 0.14
-- Case: 2-2, Probability: 0.21
-- Case: 2-3, Probability: 0.35

-- Defined data
newtype Nums = Num String
type Numbers = [Nums]
type Probabilities = [Double]

data Cube = Cube Numbers Probabilities

cube1 :: Cube
cube1 = Cube [Num "1", Num "2"] [0.3, 0.7]

cube2 :: Cube
cube2 = Cube [Num "1", Num "2", Num "3"] [0.2, 0.3, 0.5]
---------------

showPair :: Nums -> Double -> String
showPair (Num num) prob = mconcat ["Case: ",num,", Probability: ", show prob,"\n"]

instance Show Cube where
   show (Cube nums probs) = mconcat pairs
     where pairs = zipWith showPair nums probs

instance Semigroup Cube where
  (<>) cube1 (Cube [] []) = cube1
  (<>) (Cube [] []) cube2 = cube2 
  (<>) (Cube nums1 probs1) (Cube nums2 probs2) = Cube newNums newProbs
    where newNums = combineNums nums1 nums2
          newProbs = combineProbs probs1 probs2

instance Semigroup Nums where
  (<>) (Num s1) (Num "") = Num s1
  (<>) (Num "") (Num s2) = Num s2
  (<>) (Num s1) (Num s2) = Num (s1 ++ "-" ++ s2)

combineNums :: Numbers -> Numbers -> Numbers
combineNums nums1 nums2 = (<>) <$> nums1 <*> nums2

combineProbs :: Probabilities -> Probabilities -> Probabilities
combineProbs probs1 probs2 = (*) <$> probs1 <*> probs2

instance Monoid Cube where
    mempty = Cube [] []
    mappend = (<>)

main2 :: IO ()
main2 = do
  print $ cube1 <> cube2
  print $ mconcat [cube1, cube1, cube1]
