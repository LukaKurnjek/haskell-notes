
import Control.Monad
data Tree = File String | Folder String [Tree]

instance Show Tree where
  show (File s) = "File: " ++ s
  show (Folder s _) = "Folder: " ++ s

test1 :: IO ()
test1 = do
  let a = (File "test") :: Tree
      isFileEl = case a of
                   File el -> True
                   _ -> False
  when isFileEl $ print "OK"

test2 :: IO ()
test2 = do
  let treeList = [File "1", File "2"]
  printTree treeList
  print "OK"

printTree :: [Tree] -> IO ()
printTree list = go list
  where go [] = return ()
        go (x:xs) = do
          putStrLn $ show x
          go xs

main :: IO ()
main = do
  test1
  test2
