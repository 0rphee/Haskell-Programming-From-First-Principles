module Main (main) where

import Criterion.Main
import Lib


infixl 9 !?
{-# INLINABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise = 
      foldr 
        (\x r k -> case k of
                  0 -> Just x
                  _ -> r (k-1) )
        (const Nothing) xs n

myList :: [Int]
myList = [1..9999]

main :: IO ()
main = defaultMain [ bench "map list 9999"
                     $ nf (map (+1)) myList
                   ]


