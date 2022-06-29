module Main where
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck (Arbitrary (arbitrary), choose, elements)
import Control.Monad (join)

main :: IO ()
main = do
  --quickBatch (monad ([(1,2,3)] :: [(Int,Int,Int)]))
  --quickBatch (monad (ALeft (True,True,True)::AltEither Int (Bool,Bool,Bool)))
  --quickBatch (monad $ Identity ("he","ll","o"))
  quickBatch (monad (Cons (1,2,3) Nil :: List (Int,Int,Int)))
-- 18.7 Chapter Exercises
-- 1.
data Nope a = NopeDotJpg
instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

--2. 
data AltEither b a = ALeft a
                   | ARight b
                     deriving (Eq,Show)
instance Functor (AltEither b) where
  fmap _ (ARight b) = ARight b
  fmap f (ALeft a) = ALeft $ f a
instance Applicative (AltEither b) where
  pure a = ALeft a
  (ARight b) <*> _ = ARight b
  (ALeft f) <*> a = fmap f a
instance Monad (AltEither b) where
  (ALeft a) >>= f = f a
  (ARight b) >>= _ = ARight b
instance (Eq a,Eq b) => EqProp (AltEither a b) where
  (=-=) = eq
instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (AltEither b a) where
  arbitrary = do
   b <- arbitrary
   a <- arbitrary
   elements [ALeft a, ARight b]

-- 3. 
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity $ f a
instance Applicative Identity where
  pure a = Identity a
  (Identity f) <*> (Identity a) = Identity $ f a
instance Monad Identity where
  (Identity a) >>= f = f a
instance Eq a => EqProp (Identity a) where
  (=-=) = eq
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

data List a = Nil | Cons a (List a)
              deriving (Eq,Show)

append :: List a -> List a -> List a
append Nil a = a
append (Cons x xs) ys = Cons x $ append xs ys


instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)
instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xx@(Cons x xs) =
    Cons (f x) (Cons f Nil <*> xs) `append` (fs <*> xx)
instance Monad List where
  Nil >>= _ = Nil
  (Cons x xs) >>= f = append (f x) (xs >>= f)
instance Eq a => EqProp (List a) where
  (=-=) = eq
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Nil, Cons a b]

-- Functions using monad and functor methods
-- 1. 
j :: Monad m => m (m a) -> m a
j m = do
  i <- m
  i

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f as bs = do
  a <- as
  f a <$> bs
l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' f as bs = f <$> as <*> bs

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (:) <$> f x <*> meh xs f

--       ::       [m a] -> (m a -> )
flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id 



