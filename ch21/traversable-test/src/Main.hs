{-# LANGUAGE FlexibleContexts #-}

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck 

-- 21.12 Chapter Exercises
-- change the type alias for all instances being tested
type TI a = Tree a
main :: IO ()
main = do
  let trigger :: TI (Int,Int,[Int])
      trigger = undefined

--  let sTrigger :: TI Maybe (Int,Int,[Int])
--      sTrigger = undefined

  quickBatch (traversable trigger)


-- Traversable instances
-- Identity
newtype Identity a = Identity a 
                     deriving (Eq,Show)
instance Functor Identity where
  fmap f (Identity a) = Identity $ f a  
instance Foldable Identity where
  foldr f iden (Identity a) = f a iden
instance Traversable Identity where
  traverse func (Identity a) = fmap Identity (func a)
instance Eq a => EqProp (Identity a) where 
  (=-=) = eq
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- Constant
newtype Constant a b = Constant { getConstant :: a }
                       deriving (Eq, Show)
instance Functor (Constant a) where 
  fmap _ Constant {getConstant = a} = Constant {getConstant = a}
instance Foldable (Constant a) where
  foldr _ iden _ = iden
instance Traversable (Constant a) where
  traverse _ Constant {getConstant = a} = pure $ Constant {getConstant = a}
instance Eq a => EqProp (Constant a b) where
  (=-=) = eq
instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

-- Maybe
data Optional a = Nada | Yep a 
                  deriving (Eq, Show)
instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a
instance Foldable Optional where
  foldr _ iden Nada = iden
  foldr f iden (Yep a) = f a iden
instance Traversable Optional where
  traverse _ Nada = pure Nada 
  traverse f (Yep a) = fmap Yep (f a)
instance Eq a => EqProp (Optional a) where
  (=-=) = eq
instance Arbitrary a => Arbitrary (Optional a) where 
  arbitrary = do
    a <- arbitrary
    elements [Nada, Yep a]

-- List
data List a = Nil | Cons a (List a) 
              deriving (Eq, Show)
instance Functor List where 
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs
instance Foldable List where
  foldr _ iden Nil = iden
  foldr f iden (Cons x xs) = f x (foldr f iden xs)
instance Traversable List where
  traverse _ Nil = pure Nil 
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs
instance Eq a => EqProp (List a) where
  (=-=) = eq
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary 
    b <- arbitrary
    elements [Nil, Cons a b] 
  
-- Three
data Three a b c = Three a b c
                   deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
instance Foldable (Three a b) where
  foldr f iden (Three a b c) = f c iden
instance Traversable (Three a b) where
  traverse f (Three a b c) = fmap (Three a b) (f c)
instance (Eq a,Eq b,Eq c) => EqProp (Three a b c) where
  (=-=) = eq
instance (Arbitrary a,Arbitrary b,Arbitrary c) =>
          Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary

-- Pair
data Pair a b = Pair a b
                deriving (Eq, Show)
instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)
instance Foldable (Pair a) where
  foldr f iden (Pair _ b) = f b iden
instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b
instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq
instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    Pair a <$> arbitrary
  
-- Big
data Big a b = Big a b b
               deriving (Eq, Show)
instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)
instance Foldable (Big a) where
  foldMap f (Big _ b c) = f b <> f c
instance Traversable (Big a) where
  traverse f (Big a b c) = Big a <$> f b <*> f c
instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq
instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Big a b <$> arbitrary

-- Bigger
data Bigger a b = Bigger a b b b
                  deriving (Eq, Show)
instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)
instance Foldable (Bigger a) where
  foldMap f (Bigger _ b c d) = f b <> f c <> f d
instance Traversable (Bigger a) where
  traverse f (Bigger a b c d) = Bigger a <$> f b <*> f c <*> f d
instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq
instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Bigger a b c <$> arbitrary


-- S 
data S n a = S (n a) a
             deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) $ f a
instance Foldable n
      => Foldable (S n) where
  foldMap f (S n a) = foldMap f n <> f a
instance Traversable n 
      => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a  

instance (Functor n,
          Arbitrary (n a),
          Arbitrary a) =>
          Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary
instance (Applicative n,
          Testable (n Property),
          EqProp a) =>
          EqProp (S n a) where
  (S x y) =-= (S p q) = property ((=-=) <$> x <*> p)
                          .&. (y =-= q) 
              
-- Instances for Tree
data Tree a = Empty 
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a 
  fmap f (Node l a r) = 
    Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a 
  foldMap f (Node l a r) = 
    foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a 
  traverse f (Node l a r) = 
    Node <$> traverse f l <*> f a <*> traverse f r 

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    c <- arbitrary
    r <- arbitrary
    elements [Empty, Leaf a, Node l c r ]


