-- Question 1
-- Define an instance of a semigroup that multiplies the even number data type EvenNr bellow

data EvenNr = EvenNr Integer

instance Show EvenNr where
        show (EvenNr n) = show (2*n) 
		
instance Semigroup EvenNr where
        (<>) (EvenNr a) (EvenNr b) = EvenNr (a*b)

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

instance Semigroup Time where
        (<>) a b = let Sec n = timeToSec a
                       Sec m = timeToSec b
                   in secToTime (Sec (n + m)) 

-- Question 3
-- Is it possible to extend the semigroup instance of EvenNr above to a monoid?
-- Does there exist another instance of a semigroup that makes the the collection of even numbers a monoid?
-- If so, implement this semigroup and monoid.

{-
No it is not possible to extend the above semigroup instnace to a monoid instance. The even mumbers with the above
instance of <> has no idedentiy element. 

It is possible to make the collection of even number a monoid with addition as its associative operator.
-}

data EvenNr2 = EvenNr2 Integer

instance Show EvenNr2 where
        show (EvenNr2 n) = show (2*n)

instance Semigroup EvenNr2 where
        (<>) (EvenNr2 a) (EvenNr2 b) = EvenNr2 (a+b)

instance Monoid EvenNr2 where
        mempty = EvenNr2 0
        mappend = (<>)


-- Question 4
-- Define an instance of a monoid for the Time data type.

instance Monoid Time where
        mempty = Time (Hour 0, Min 0, Sec 0)
        mappend = (<>)
