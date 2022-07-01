import Data.Foldable
-- Chapter 20: Foldable
-- 20.2 The Foldable class

foldWithMonoid :: [Char]
foldWithMonoid = fold ["kjakjkjdf", ",dfs", "dfs"]

-- Identity
data Identity a = Identity a deriving (Eq,Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

-- Maybe
data Optional a = None | May a deriving (Eq,Show)
instance Foldable Optional where
  foldr _ i None = i
  foldr f i (May a) = f a i

  foldl _ i None = i
  foldl f i (May a) = f i a

  foldMap _ None = mempty
  foldMap f (May a) = f a

-- Exercises Library Functions
-- 1.
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

-- 3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = foldl helper False
  where helper True _ = True
        helper False ie = e == ie

-- 4. 
reducer :: (a -> b -> b) -> Maybe a -> b -> Maybe b
reducer f Nothing x = Just x
reducer f (Just x) y = Just (f x y)

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldl helper Nothing
  where helper = reducer min

-- 5. 
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldl helper Nothing
  where helper = reducer max

-- 6.
null' :: (Foldable t) => t a -> Bool
null' = foldl (\_ _ -> False) True

-- 7. 
length' :: (Foldable t) => t a -> Int
length' = foldl (\a _ -> a +1) 0

-- 8.
toList' :: (Foldable t) => t a ->  [a]
toList'= foldl (flip (:)) []

-- 9.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id 

-- 20.6 Chapter Exercises
-- 1.
data Constant a b = Constant b deriving (Eq,Show)
instance Foldable (Constant a) where
  foldr f d (Constant b) = f b d

-- 2.
data Two a b = Two a b deriving (Eq, Show)
instance Foldable (Two a) where
  foldr f d (Two _ b) = f b d

-- 3.
data Three a b c = Three a b c deriving (Eq,Show)
instance Foldable (Three a b) where
  foldr f d (Three _ _ c) = f c d
 
-- 4. 
data Three' a b = Three' a b b deriving (Eq, Show)
instance Foldable (Three' a) where
  foldr f d (Three' _ b c) = f c $ f b d 

-- 5.
data Four a b = Four a b b b deriving (Eq, Show)
instance Foldable (Four a) where
  foldr f x (Four _ b c d) = f d $ f c $ f b x

-- Filter function for Foldables with foldMap
filterF :: (Applicative f, Foldable t, Monoid (f a)) =>
            (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty) 
