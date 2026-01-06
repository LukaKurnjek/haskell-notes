
module Libraries (parseNumber) where

import Data.Scientific (Scientific)

parseNumber :: String -> (Bool, Scientific) 
parseNumber inp = (validity, n)
  where validity = checkIfScientific inp
        n = if validity
            then read inp :: Scientific
            else 0 :: Scientific

checkIfScientific :: String -> Bool 
checkIfScientific inp = and [check1, check2, check3, check4] 
  where check1 = all (`elem` "0123456789e-") inp
        check2 = '-' `notElem` drop 1 inp
        check3 = 'e' `notElem` [head inp,last inp]
        check4 = 1 == (length . filter (== 'e')) inp

