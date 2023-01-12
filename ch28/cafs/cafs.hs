module Main where


incdInts :: [Integer] -> [Integer]
incdInts = map (+ 1)

addition x = (1+)

main :: IO ()
main = do
  print (incdInts [1 ..] !! 1000)

-- >>> show 13
--

  
