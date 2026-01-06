-- Question 1
-- Define an instance of a semigroup that multiplies the even number data type EvenNr bellow

data EvenNr = EvenNr Integer

instance Show EvenNr where
        show (EvenNr n) = show (2*n) 

-- Question 2
-- Definan an instance of a semigroup that adds time for the Time data type bellow

newtype Hour = Hour Int deriving Show
newtype Min = Min Int deriving Show
newtype Sec = Sec Int deriving Show
data Time = Time (Hour, Min ,Sec) deriving Show

secToTime :: Sec -> Time
secToTime (Sec x) = let h = (x `div` 3600) `mod` 24
                        m = (x `mod` 3600) `div` 60
                        s = x `mod` 60
                  in Time (Hour h, Min m, Sec s)

timeToSec :: Time -> Sec
timeToSec (Time (Hour h, Min m, Sec s)) = Sec (h*3600+m*60+s)

-- Question 3
-- Is it possible to extend the semigroup instance of EvenNr above to a monoid?
-- Does there exist another instance of a semigroup that makes the the collection of even numbers a monoid?
-- If so, implement this semigroup and monoid.

-- Question 4
-- Define an instance of a monoid for the Time data type.

