import Data.Char

acron :: [Char] -> [Char]
acron xs = [x | x <- xs, x `elem` ['A'..'Z']]

mySqr = [x^2 | x <- [1..5]]

myCube = [y^3 | y <- [1..5]]


tups :: [(Integer, Integer)]
tups = [(x, y) | x <- mySqr, x < 50, y <- myCube, y < 50, odd x]


-- Zipping exercises
myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (a->b->c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

-- Exercises
capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = toUpper x:xs

-- Recursive Capitalization of String
capAll :: [Char] -> [Char]
capAll [] = []
capAll (x:xs) = toUpper x: capAll xs

firstCap :: [Char] -> Char
firstCap [] = ' '
firstCap (x:_) = toUpper x