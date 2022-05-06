module Factorial where

factorial :: (Eq p, Num p) => p -> p
factorial 0 = 1
factorial n = n * factorial (n-1)

-- 8.6 Chapter Exercises
-- Recursion
-- 2.

sumTo :: (Eq p, Num p) => p -> p
sumTo 1 = 1
sumTo n = n + sumTo (n-1)

-- 3. 

recMulti :: (Integral a) => a -> a -> a
recMulti _ 0 = 0
recMulti 0 _ = 0
recMulti x y = x + recMulti x (y-1) 

-- McCarthy 91 function
mC91 n 
    | n > 100 = n-10
    | otherwise = mC91 (mC91 (n+11))