-- Chapter 16: Functor

data BTree a = Leaf | Node (BTree a) a (BTree a)
  deriving (Show,Eq)
instance Functor BTree where
  fmap _ Leaf = Leaf
  fmap f (Node l val r) = Node (fmap f l) (f val) (fmap f r)