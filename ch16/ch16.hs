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

