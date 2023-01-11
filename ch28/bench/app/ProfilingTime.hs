module Main where

f :: IO ()
f = do
  print ([(1 :: Int)..] !! 999999 )
  putStrLn "f"

g :: IO ()
g = do
  print ([(1 :: Int)..] !! 9999999 )
  putStrLn "g"
  
main :: IO ()
main = do
  f
  g
  
  
  
