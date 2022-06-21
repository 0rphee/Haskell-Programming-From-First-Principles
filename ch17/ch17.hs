import Control.Applicative (liftA3)
a = Just 5
b = Just 3
c = Just 6
d = Just 9

e = liftA3 f a b c

f :: Num a => a -> a -> a -> a
f a b c = sum [a,b,c]

g = fmap (<>"hello") (Just "a")

