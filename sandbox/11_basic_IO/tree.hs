
import Data.List
import Control.Monad
import System.Directory (doesFileExist, listDirectory)

data Tree = File String | Folder String [IO Tree]
type Level = Integer

instance Show Tree where
  show (File s) = "File: " ++ s
  show (Folder s _) = "Folder: " ++ s

main :: IO ()
main = do
  putStrLn "."
  s <- returnStructure "."
  printTree s 0
  putStrLn "OK"
  printTree2 s 0
  putStrLn "OK"
  printTreeTest1 s
  printTreeTest2 s

returnStructure :: FilePath -> IO [IO Tree]
returnStructure filePath = do
  contents <- listDirectory filePath
  return $ map go contents
 where
  go fileName = do
    let newFilePath = filePath ++ "/" ++ fileName
    isFile <- doesFileExist newFilePath
    if isFile
      then return $ File fileName
      else do
        structure <- returnStructure newFilePath
        return $ Folder fileName structure

printTree :: [IO Tree] -> Level -> IO ()
printTree [] _ = return ()
printTree (x:xs) l = do
  t <- x
  printSpaces l
  printElement xs t
  case t of
    Folder name ioList -> do
      printTree ioList (l + 1)
    File _ -> return ()
  printTree xs l

printTree2 :: [IO Tree] -> Level -> IO ()
printTree2 [] _ = return ()
printTree2 (x:xs) l = do
  structure <- x
  case structure of
    File name -> do
      printSpaces l
      printElement2 xs name
    Folder name ioList -> do
      printSpaces l
      printElement2 xs name
      printTree2 ioList (l + 1)
  printTree2 xs l

printSpaces :: Level -> IO ()
printSpaces l = do
  if l < 1 then return ()
  else do
    putStr "    "
    printSpaces $ l - 1

printElement :: [IO Tree] -> Tree -> IO ()
printElement xs t = do
  let name = case t of
               File n -> n
               Folder n _ -> n
  if null xs then putStrLn $ "└── " ++ name
  else putStrLn $ "├── " ++ name

printElement2 :: [IO Tree] -> String -> IO ()
printElement2 xs name =
  if null xs
    then putStrLn $ "└── " ++ name
    else putStrLn $ "├── " ++ name

-- Test functions
-----------------------------------------
printTreeTest1 :: [IO Tree] -> IO ()
printTreeTest1 l = do
  forM_ l $ \li -> do
      el <- li
      putStrLn $ show el
  print "OK"

printTreeTest2 :: [IO Tree] -> IO ()
printTreeTest2 l = go l
  where go [] = print "OK"
        go (x:xs) = do
          t <- x
          putStrLn $ show t
          go xs
-----------------------------------------

