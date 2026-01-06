
-- For question 1
import Control.Monad.Reader (asks, runReader, MonadReader(ask), Reader)
import qualified Data.Map as Map
-- For question 2
import Data.List (intercalate)
import System.IO (writeFile)

-- Question 1
-- Write a function that takes in a dictionary of type Map String Int, where the first element 
-- is "count": n, and n represents the lenght of the Map. The function should run a Reader monad
-- that is parameterize by this Map and returns a bool, which says weather the count number in 
-- the map is representing the actual length of the Map. Try to use the ask and asks functions.

type Dictionary = Map.Map String Int;

checkMapCount :: Dictionary -> Bool
checkMapCount = runReader checkDict

checkDict :: Reader Dictionary Bool
checkDict = do
    count <- asks (lookupMapVar "count")
    dictionary <- ask
    return (count == Map.size dictionary)

lookupMapVar :: String -> Dictionary -> Int
lookupMapVar name dictionary = maybe 0 id (Map.lookup name dictionary)

dictionary1 :: Map.Map String Int
dictionary1 = Map.fromList [("count",3), ("1",1), ("2",2)]

main1 :: IO ()
main1 = do
    putStr $ "Count for dictionary " ++ show (Map.toList dictionary1) ++ " is correct: "
    print (checkMapCount dictionary1)

-- Question 2
-- Write a program that asks the user for his name and generater a HTML document that
-- displays a simple web-page with his name. Use the Reader monad. Below you can see 
-- an example of the HTML document for the user name User1.

-- <!DOCTYPE html>
-- <html lang="en">
--   <body>
--     <h1>Your site</h1><h3>Hello User1!</h3>
--   </body>
-- </html>

type Html = String
type Name = String

main2 :: IO ()
main2 = do
  putStrLn "Input your name:"
  name <- getLine
  case name of
    "" -> do
      putStrLn "You must provide at least one character:"
    _ -> do
      writeFile filePath . generateHtmlDocContent $ runReader page name
      putStrLn $ "Written HTML file to file \"" ++ filePath ++ "\"."
  where
    filePath = "mySite.html"

page :: Reader Name Html
page = do
  content' <- content
  return $ combine [topNav, content']

topNav :: Html
topNav = h1 ["Your site"]

content :: Reader Name Html
content = do
  name <- ask
  return $ h3 ["Hello " ++ name ++ "!"]

combine :: [Html] -> Html
combine = intercalate ""

h1 :: [Html] -> Html
h1 children =
  "<h1>" ++ combine children ++ "</h1>"

h3 :: [Html] -> Html
h3 children =
  "<h3>" ++ combine children ++ "</h3>"

generateHtmlDocContent :: Html -> Html
generateHtmlDocContent html =
  "<!DOCTYPE html>\n\
    \<html lang=\"en\">\n\
    \\t<body>\n"
  ++ "\t\t" ++ html
  ++ "\n\t</body>\n\
    \</html>\n"
