-- 3.8 Chapter Exercises
-- Building Functions 2.

aFunc str = str ++ "!"

bFunc str = str !! 4

cFunc = drop 9

-- Building Functions 3.
thirdChar :: [Char] -> Char 
thirdChar = flip (!!) 2

-- Building Functinos 4.
charFromIndex = (!!) "Curry is awesome!"

-- Building Functions 5.
rvrs str = awes ++ is ++ cur
     where awes = take 7 (drop 9 str)
           is = take 4 (drop 5 str)
           cur = take 5 str