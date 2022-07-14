module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM, join)
import Control.Monad.Trans.State
import System.Random
import RandomExample

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 7)
  return (intToDie n, s) 

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- Exercises: Roll Your Own
-- 1.
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0 
  where go :: Int -> Int -> StdGen -> Int
        go s c gen
          | s >= n = c
          | otherwise = go (s + die) 
                           (c + 1) nextGen
          where (die, nextGen) = 
                 randomR (1, 7) gen

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty =  rollsToGetN 20

-- 2.
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 (0, [])
  where go :: Int -> (Int, [Die] )-> StdGen -> (Int, [Die])
        go s (c, xs) gen
          | s >= n = (c, xs)
          | otherwise = go (s + dieN)
                           (c+1, intToDie dieN : xs )
                            nextGen
          where (dieN, nextGen) = 
                 randomR (1, 7) gen


-- 23.6 Write State for youself
newtype Moi s a =  Moi { runMoi :: s -> (a, s) }  

instance Functor (Moi s) where 
  fmap f (Moi g) = Moi $ \r -> let (a, b) = g r   
                               in (f a, b)

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)
  (Moi f) <*> (Moi g) = Moi $ \r -> let (l, n) = f r 
                                        (a, n') = g n
                                    in (l a, n')

instance Monad (Moi s) where
  (Moi f) >>= g = Moi $ 
                  \ s -> let (a, s') = f s
                             (Moi sb) = g a
                         in sb s'


