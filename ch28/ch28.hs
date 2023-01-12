-- 28.10 Chapter Exercises
-- Difference List

newtype DList a = DL 
  { unDL :: [a] -> [a] } 

empty :: DList a
empty = undefined
{-# INLINE empty #-}

singleton :: a -> DList a
singleton = undefined
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = undefined
{-# INLINE toList #-}

infixr `cons` 
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc = undefined
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a 
append = undefined
{-# INLINE append #-}

