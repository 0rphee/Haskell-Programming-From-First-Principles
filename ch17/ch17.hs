import Control.Applicative (liftA3, Applicative (liftA2), liftA)
import Data.List (elemIndex)
a = Just 5
b = Just 3
c = Just 6
d = Just 9

e = liftA3 f a b c

f :: Num a => a -> a -> a -> a
f a b c = sum [a,b,c]

g = fmap (<>"hello") (Just "a")

fun x = lookup x [ (3, "hello")
               , (4, "julie")
               , (5, "kbai")]

gun y = lookup y [ (7, "sup?")
               , (8, "chris")
               , (9, "aloha")]
h z = lookup z [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]

-- Exercises: Lookups
-- 1.
added :: Maybe Integer
added = (+3) <$> lookup  3 (zip [1,2,3] [4,5,6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

ys :: Maybe Int
ys = elemIndex 4 [1, 2, 3, 4, 5]


max' :: Maybe Int -> Maybe Int -> Maybe Int
max' = max

maxed :: Maybe Int
maxed = max' x ys

-- 4.

hh = [1,2,3]
kk = [4,5,6]

p :: Maybe Integer
p = lookup 3 $ zip hh kk

q :: Maybe Integer
q = lookup 3 $ zip hh kk

summed :: Maybe Integer
summed = (+) <$> p <*> q

newtype Identity a = Identity a
  deriving (Eq, Show, Ord)
instance Functor Identity where
  fmap f (Identity a) = Identity $ f a
instance Applicative Identity where
  pure a = Identity a
  (Identity a) <*> (Identity b) = Identity $ a b

-- Exercise: Constant Instance
newtype Constant a b =
  Constant { getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a
instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  (Constant a) <*> (Constant b) = Constant (a <> b)

-- Exercise: Fixer Upper
-- 1.
fix1 :: Maybe [Char]
fix1 = const <$> Just "Hello" <*> Just "World"

fix2 :: Maybe (Integer, Integer, [Char], [Integer])
fix2 = (,,,) <$> Just 90 <*> Just 10 
      <*> Just "Tierness" <*> Just [1,2,3]

-- Exercises
-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos = liftA3 helper
  where helper :: a -> b -> c -> (a,b,c)
        helper a b c = (a,b,c)