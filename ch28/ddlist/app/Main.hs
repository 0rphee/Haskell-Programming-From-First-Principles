module Main (main) where

import Lib
import Criterion.Main
import qualified Data.Sequence as S
import Data.Sequence ((<|))

-- 28.10 Chapter Exercises
-- Difference List

newtype DList a = DL
  { unDL :: [a] -> [a] }

empty :: DList a
empty = DL $ const []
{-# INLINE empty #-}

singleton :: a -> DList a
singleton a = DL $ const [a]
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList (DL dl) = dl []
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x (DL xs )= DL ((x:) . xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc (DL xs) x = DL ( (++ [x]) . xs )
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append (DL xs) (DL ys) = DL (xs . ys)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) (n : xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs =
          go (n-1) (singleton n `append` xs)

ddlist :: IO ()
ddlist = defaultMain
  [ bench "concat list" $
    whnf schlemiel 123456
  , bench "concat dlist" $
    whnf constructDlist 123456]

--------------------------------------------------
--------------------------------------------------
data Queue a = Queue { enqueue :: [a]
                     , dequeue :: [a]
                     } deriving (Eq, Show)
push :: a -> Queue a -> Queue a
push item (Queue xs ys) = Queue (item:xs) ys

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue xs []) = let ys = reverse xs
                    in Just ( head ys, Queue [] (tail ys) )
pop (Queue xs (y:ys)) = Just ( y, Queue xs ys )

data SQueue a = SQueue [a] Bool -- True -> Pushable
  deriving (Eq, Show)

pushS :: a -> SQueue a -> SQueue a
pushS item (SQueue xs True) = SQueue (item:xs) True
pushS item (SQueue xs False) = SQueue (item: reverse xs) True

popS :: SQueue a -> Maybe (a, SQueue a)
popS (SQueue [] _) = Nothing
popS (SQueue xs True) = Just (head ys, SQueue (tail ys) False)
  where ys = reverse xs
popS (SQueue (x:xs) False) = Just (x, SQueue xs False)

singleQueue :: Int -> SQueue Int
singleQueue i = go i True (SQueue [] True)
  where go :: Int -> Bool -> SQueue Int -> SQueue Int
        go 0 _ xs = xs
        go n True xs = go (n-1) False $ pushS n xs
        go n False xs = go (n-1) True $ case popS xs of
                                        Just (_, nq) -> nq
                                        Nothing -> xs
doubleQueue :: Int -> Queue Int
doubleQueue i = go i True (Queue [] [])
  where go :: Int -> Bool -> Queue Int -> Queue Int
        go 0 _ xs = xs
        go n True xs = go (n-1) False $ push n xs
        go n False xs = go (n-1) True $ case pop xs of
                                        Just (_, nq) -> nq
                                        Nothing -> xs
---------------------------------------
---------------------------------------
-- Benchmarking against Sequence
pushSeq :: a -> S.Seq a -> S.Seq a
pushSeq = (<|)

-- TODO finish benchmarks against Sequence
-- tSequence :: Int -> S.Seq Int
-- tSequence i = go i True S.empty
--   where go :: Int -> Bool -> S.Seq Int -> S.Seq Int
--         go 0 _ xs = xs
--         go n True xs = go (n-1) False $ push n xs
--         go n False xs = go (n-1) True $ case pop xs of
--                                         Just (_, nq) -> nq
--                                         Nothing -> xs

queue :: IO ()
queue = defaultMain
  [
    bench "single list queue" $
    whnf singleQueue 123456
  , bench "double list queue" $
    whnf doubleQueue 123456
  ]


main :: IO ()
main = queue
