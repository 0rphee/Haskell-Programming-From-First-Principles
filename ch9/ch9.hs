acron :: [Char] -> [Char]
acron xs = [x | x <- xs, x `elem` ['A'..'Z']]

mySqr = [x^2 | x <- [1..5]]

myCube = [y^3 | y <- [1..5]]


tups :: [(Integer, Integer)]
tups = [(x, y) | x <- mySqr, x < 50, y <- myCube, y < 50, odd x]

