{-
-- Question 1 --
Continuing with the logistics software of the lesson:
 1. After using the `Container` type class for a while, you realize that it might need a few adjustments:
  	- First, write down the `Container` type class and its instances, same as we did in the lesson
  	  (try to do it without looking and check at the end or if you get stuck).
  	- Then, add a function called `unwrap` that gives you back the value inside a container.
 2. Create an instance for the `MailedBox` data type.
 	- The MailedBox data type represents a box sent through the mail.
 	- The parameter `t` is a tag with a person's identifier
 	- The parameter `d` is the person's details (address,etc).
 	- The parameter `a` is the content of the MailedBox
-}

data MailedBox t d a = EmptyMailBox t d | MailBoxTo t d a

data Box a = Empty | Has a

data Present t a = EmptyPresent t | PresentFor t a

-- Container type class
class Container c where
  isEmpty :: c a -> Bool
  contains :: (Eq a) => c a -> a -> Bool
  replace :: c a -> b -> c b
  unwrap :: c a -> a -> a

-- Container instance for Box type
instance Container Box where
  isEmpty Empty = True
  isEmpty _ = False

  contains Empty _ = False
  contains (Has x) y = x == y

  replace _ = Has -- Point-free version of: replace _ x = Has x

  unwrap Empty x = x
  unwrap (Has x) _ = x

-- Container instance for (Present t) type
instance Container (Present t) where
  isEmpty (EmptyPresent _) = True
  isEmpty _ = False

  contains (EmptyPresent _) _ = False
  contains (PresentFor _ x) y = x == y

  replace (EmptyPresent t) x = PresentFor t x
  replace (PresentFor t _) x = PresentFor t x

  unwrap (EmptyPresent _) x = x
  unwrap (PresentFor _ x) _ = x

-- Container instance for (MailedBox t d) type
instance Container (MailedBox t d) where
  isEmpty (EmptyMailBox _ _) = True
  isEmpty _ = False

  contains (EmptyMailBox _ _) _ = False
  contains (MailBoxTo _ _ x) y = x == y

  replace (EmptyMailBox t d) x = MailBoxTo t d x
  replace (MailBoxTo t d _) x = MailBoxTo t d x

  unwrap (EmptyMailBox _ _) x = x
  unwrap (MailBoxTo _ _ x) _ = x

-- Question 2 --
-- Create instances of Show, Eq, and Ord for these three data types (use
-- automatic deriving whenever possible):

{-
We can safely derive Show virtually always.
We can safely derive Eq for Position because the value constructors have no parameters.
We can safely derive Ord for Position because the value constructors have no parameters.
-}
data Position = Intern | Junior | Senior | Manager | Chief deriving (Show, Eq, Ord)

{-
We can safely derive Show virtually always.
We can safely derive Eq for Experience because the value constructors have no parameters.
We can safely derive Ord for Experience because the value constructors have no parameters.
-}
data Experience = Programming | Managing | Leading deriving (Show, Eq, Ord)

-- No deriving here. Address is just another name for String
type Address = String

{-
We can safely derive Show virtually always.
We shouldn't derive Eq because there is a relationship between USD, EUR, and ADA.
We shouldn't derive Ord because there is a relationship between USD, EUR, and ADA.
-}
data Salary = USD Double | EUR Double deriving (Show)

{-
Helper value to make it easier to define the instances.
Here, we set a fixed value, but in a real app, we would use an API to continuously
get an updated value. We'll learn how to do this in future lessons.
-}
rateUsdEur = 1.1

instance Eq Salary where
  USD x == USD y = x == y
  EUR x == EUR y = x == y
  USD x == EUR y = x * rateUsdEur == y
  EUR x == USD y = x == y * rateUsdEur

instance Ord Salary where
  USD x `compare` USD y = compare x y
  EUR x `compare` EUR y = compare x y
  USD x `compare` EUR y = compare (x * rateUsdEur) y
  EUR x `compare` USD y = compare x (y * rateUsdEur)

{-
We can safely derive Show virtually always.
We can safely derive Eq for Relationship because the value constructors don't have an
implicit relationship.
We can safely derive Ord for Relationship because the value constructors don't have an
implicit relationship.
-}
data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address
  deriving (Show, Eq, Ord)

{-
- We can safely derive Show virtually always.
- We can safely derive Eq for Pokemon. We could implement a more efficient Eq instance
  that only compares the pPokedexNum or pName (since that's unique for a Pokemon). But
  there's no need since two values of the same pokemon should have all fields equal.
- We shouldn't derive Ord because it would compare all the fields lexicographically. In
  that case, "Charizard" (#6) would come before "Venusaur" (#3). Because the name is
  the first field to be compared, and "C" comes before "V." We both know that's unacceptable,
  so we have to compare only the PokeDex number.
-}
data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  }
  deriving (Show, Eq)

instance Ord Pokemon where
  compare Pokemon {pPokeDexNum = p1} Pokemon {pPokeDexNum = p2} = compare p1 p2

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3

-- Question 3 -- EXTRA CREDITS
-- Uncomment the next code and make it work (Google what you don't know).

-- Team memeber experience in years
newtype Exp = Exp Double deriving (Show)

-- Team memeber data
type TeamMember = (String, Exp)

-- List of memeber of the team
team :: [TeamMember]
team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]

-- Function to check the combined experience of the team
-- This function applied to `team` using GHCi should work
combineExp :: [TeamMember] -> Exp
combineExp = foldr ((+) . snd) 0

instance Num Exp where
  (+) (Exp x) (Exp y) = Exp (x + y)
  (*) (Exp x) (Exp y) = Exp (x + y)
  fromInteger x = Exp (fromInteger x)
  signum (Exp x) = Exp (signum x)
  abs (Exp x) = Exp (abs x)
  negate (Exp x) = Exp (negate x)
