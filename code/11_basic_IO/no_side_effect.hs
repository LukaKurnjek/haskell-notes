import System.Directory (listDirectory)

main :: IO ()
main = do
  x <- listDirectory "."
  print x

  -- does not create msg.txt
  let a = f 1
  print a

  {- does create msg.txt
  let b = writeFile "./msg.txt" ""
  c <- b -}

  y <- listDirectory "."
  print y

f :: Int -> Maybe Int
f n = do
  let b = writeFile "./msg.txt" ""
  b `seq` return (n + 1)
