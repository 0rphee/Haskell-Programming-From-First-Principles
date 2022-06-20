{-# LANGUAGE FlexibleInstances #-}

-- Chapter 16: Functor

data BTree a = Leaf | Node (BTree a) a (BTree a)
  deriving (Show,Eq)
instance Functor BTree where
  fmap _ Leaf = Leaf
  fmap f (Node l val r) = Node (fmap f l) (f val) (fmap f r)


-- 16.6 The Good the Bad, and the Ugly
data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled
                  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)


e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap ((read . ("123"++)) . show) ioi
    in fmap (*3) changed

-- 16.10
-- 1.
newtype Identity a = Identity a deriving Show
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- 2.
data Pair a = Pair a a deriving Show
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

-- 3.
data Two a b = Two a b deriving Show
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- 4.
data Three a b c = Three a b c deriving Show
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

-- 5.
data Three' a b = Three' a b b deriving Show
instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

-- 6.
data Four a b c d = Four a b c d deriving Show
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

-- Exercise: Possibly
data Possibly a = LolNope | Yeppers a deriving (Eq,Show)
instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

data Sum a b = First a | Second b deriving (Eq,Show)
instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

-- 16.7 Chapter exercises
-- 1. 
data Quant a b = Finance
               | Desk a
               | Bloor b deriving Show
instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

--3.
newtype Flip f a b = Flip (f b a) deriving (Eq,Show)
newtype K a b = K a deriving (Eq,Show)
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

-- 4. 
data EvilGoateeConst a b = GoatyConst b deriving Show
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

-- 6.
data Parappa f g a = DaWrappa (f a) (g a) deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa l r) = DaWrappa (fmap f l) (fmap f r)

-- 7.
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b) deriving Show
instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething l r) =
    IgnoringSomething l (fmap f r)

-- 8.
data Notorious g o a t =
  Notorious (g o) (g a) (g t) deriving Show
instance Functor g => Functor (Notorious g o a) where
  fmap func (Notorious f s t) = Notorious f s (fmap func t)

-- 9.
data List a = Nil | Cons a (List a) deriving Show
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

-- 10.
data GoatLord a = NoGoat 
                | OneGoat a
                | MoreGoats (GoatLord a)
                            (GoatLord a)
                            (GoatLord a)
                deriving Show
instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a)
                                       (fmap f b)
                                       (fmap f c) 

-- 11.
data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (f . g) 
instance (Show a) => Show (TalkToMe a)  where  
  show Halt = "Halt"
  show (Print s a) = "Print " ++ s ++ " " ++ show a
  show (Read f) = "Read f"
