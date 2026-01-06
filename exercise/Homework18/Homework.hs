

-- Question 1
-- Write a function that takes in an aritmetic operator that has an instance of Fractional as +, -, 
-- * or /. It also takes in a list of type [Double]. Then it calulates the number of all possible 
-- computations where you can take any of two elements from the list and uses the provided operator 
-- on them. For which of the stated operators above is the number the smallest for the list [1..5]? 


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

