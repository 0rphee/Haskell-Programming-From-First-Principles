-- Chapter 22: Reader 
import Data.Maybe
import Control.Applicative 
import Data.Char (toUpper)

boop = (*2)
doop = (+10)

bip :: Integer -> Integer 
bip = boop . doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop  

-- Short Exercise: Warming Up
cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev = reverse 

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char]) 
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char]) 
tupled' = do
  a <- cap
  b <- rev
  return (a,b)

tupled'' :: [Char] -> ([Char], [Char]) 
tupled'' = rev <$> cap >>= (,)

newtype Reader r a = 
  Reader { runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure a = Reader $ \_ -> a 
  (Reader f) <*> (Reader g) = 
    Reader $ \r -> f r (g r)   

-- same as (.)
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- Exercise: Ask
ask :: Reader a a
ask = Reader id

-- 22.6 Functions have an Applicative too
newtype HumanName = HumanName String
  deriving (Eq, Show)

newtype DogName = DogName String
  deriving (Eq, Show)

newtype Address = Address String
  deriving (Eq, Show)

data Person = Person {
    humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
  } deriving (Eq, Show)


data Dog = Dog {
    dogsName    :: DogName
  , dogsAddress :: Address 
  } deriving (Eq, Show)


pers :: Person
pers = Person (HumanName "Big Bird") 
              (DogName "Barkley")
              (Address "Sesame Street")

chris :: Person 
chris = Person (HumanName "Chris Allen")
               (DogName "Papu") 
               (Address "Austin")

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address 

(<$->>) :: (a -> b) -> (r -> a) -> (r -> b)
(<$->>) = (<$>)

(<*->>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
(<*->>) = (<*>)

getDogR' :: Person -> Dog
getDogR' = Dog <$->> dogName <*->> address 

-- with Reader, alternate
getDogR'' :: Person -> Dog
getDogR'' = liftA2 Dog dogName address

-- Exercise: Reading Comprehension
myLiftA2 :: Applicative f =>
            (a -> b -> c) 
         -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b 

asks :: (r -> a) -> Reader r a
asks = Reader

-- 22.7 The Monad of functions
foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

fooBind :: (r -> a) 
        -> (a -> r -> b)
        -> (r -> b)
fooBind m k = \r -> k (m r) r 

-- The Monad instance
-- Example uses of the Reader type
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

-- Exercise: Reader Monad
-- 1. Implement Reader Monad
instance Monad (Reader r) where
  (Reader ra) >>= aRb = 
    Reader $ \r -> let (Reader rb) = aRb $ ra r
                   in rb r

-- 2. Rewrite getDogRm to use Reader
getDogRMM :: Reader Person Dog
getDogRMM = do
  name <- Reader dogName
  addy <- Reader address
  return $ Dog name addy

-- 22.11 Chapter Exercises
-- A warm-up stretch
x :: [Integer]
x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

bolt2 :: Integer -> Bool
bolt2 = (&&) <$> (>3) <*> (<8)

main :: IO ()
main = do
  print $ 
    sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ foldr (&&) True $ seqA 3
  print $ seqA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys

seqA :: Integral a => a -> [Bool]
seqA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

