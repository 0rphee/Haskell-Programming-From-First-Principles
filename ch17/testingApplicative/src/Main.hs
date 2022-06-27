module Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (ZipList)

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

newtype ZipList' a = ZipList' (List a)
                     deriving (Eq, Show) 
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l
instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs
instance Applicative ZipList' where
  pure a = ZipList' $ pure a
  (<*>) (ZipList' fs) (ZipList' as) =
    ZipList' $ internal  fs as
    where internal :: List (a -> b) -> List a -> List b
          internal Nil _ = Nil
          internal _ Nil = Nil
          internal (Cons f Nil) (Cons b bs) = Cons (f b) (f <$> bs)
          internal (Cons f fs) bs@(Cons b Nil) = Cons (f b) (fs <*> bs)
          internal (Cons f fs) (Cons b bs) = Cons (f b) (internal fs bs)  
instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary
instance Semigroup a => Semigroup (ZipList' a) where
  (<>) a (ZipList' Nil) = a
  (<>) (ZipList' Nil) a = a
  (<>) (ZipList' (Cons a as)) (ZipList' (Cons b bs)) =
    ZipList' $ Cons (a<>b) (as<>bs)


-- ZipList Applicative Exercise
take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs) 


main :: IO ()
main = do
  quickBatch (monoid Twoo)
  quickBatch (applicative (Cons (True,True,True) Nil))
  quickBatch (applicative (ZipList' (Cons (True,True,True) Nil)))