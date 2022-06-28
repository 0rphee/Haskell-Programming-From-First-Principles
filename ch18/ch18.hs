{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<" #-}
import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
  then [x*x,x*x]
  else []

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = xs >>= f 
  where f y = if even y
              then [y*y,y*y]
              else []

-- years ago
type Founded = Int
-- number of programmers
type Coders = Int

data SoftwareShop = 
  Shop {
      founded     :: Founded
    , programmers :: Coders
  } deriving (Eq,Show)

data FoundedError = NegativeYears Founded
                  | TooManyYears Founded
                  | NegativeCoders Coders
                  | TooManyCoders Coders
                  | TooManyCodersForYears Founded Coders
                  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Int
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Int
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
  then Left $ TooManyCodersForYears founded programmers
  else Right $ Shop founded programmers

  -- Short Exercise: Either Monad
data Sum a b = First a 
             | Second b deriving (Eq, Show)
instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b
instance Monoid a => Applicative (Sum a) where
  pure a = Second a
  (<*>) (First a) (First b) = First (a <> b)
  (<*>) (First a) _ = First a
  (<*>) _ (First b) = First b
  (<*>) (Second f) (Second a) = Second $ f a
instance Monoid a => Monad (Sum a) where
  (>>=) (First a) _ = First a
  (>>=) (Second b) f = f b
