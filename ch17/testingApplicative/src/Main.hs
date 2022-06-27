module Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo
            deriving (Eq,Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1,return Fools)
                        ,(1,return Twoo)]
instance Semigroup Bull where
  (<>) _ _ = Fools
instance Monoid Bull where
  mempty = Fools
instance EqProp Bull where
  (=-=) = eq

-- List Applicative exercise
data List a = Nil | Cons a (List a)
              deriving (Eq, Show)
append :: List a -> List a -> List a
append Nil a = a
append (Cons x xs) ys = Cons x $ xs `append` ys

toMyList :: [a] -> List a
toMyList = foldr Cons Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)
instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xx@(Cons x xs) =
    Cons (f x) (Cons f Nil <*> xs) `append` (fs <*> xx)
instance Semigroup a => Semigroup (List a) where
  (<>) a Nil = a
  (<>) Nil a = a
  (<>) (Cons x xs) (Cons y ys) = Cons (x<>y) (xs<>ys)
instance Monoid a => Monoid (List a) where
  mempty = Nil
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    as <- arbitrary
    elements [Nil, Cons a as]
instance Eq a => EqProp (List a) where 
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (monoid Twoo)
  quickBatch (applicative (Cons (True,True,True) Nil))