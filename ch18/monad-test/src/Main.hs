module Main where
import Test.QuickCheck.Checkers (quickBatch)
import Test.QuickCheck.Classes (monad)

main :: IO ()
main = do
  quickBatch (monad ([(1,2,3)] :: [(Int,Int,Int)]))

-- 18.7 Chapter Exercises
