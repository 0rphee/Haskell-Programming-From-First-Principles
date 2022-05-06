-- Pattern matching
isTwo :: (Eq a, Num a) => a -> Bool
isTwo 2 = True
isTwo _ = False

data WherePenguinsLive =
      Galapagos
    | Antartica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

newtype Penguin = Peng WherePenguinsLive
                  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng home) = home

galapagosPeng :: Penguin -> Bool
galapagosPeng (Peng Galapagos) = True
galapagosPeng _                = False

antarcticPeng :: Penguin -> Bool
antarcticPeng (Peng Antartica) = True
antarcticPeng _                = False

antarcticOrGalapagos p = galapagosPeng p
                      || antarcticPeng p
-- Exercises: Variety Pack
-- 2
f :: (a,b,c) -> (d,e,f) -> ((a,d), (c,f))
f (a,b,c) (d,e,f) = ((a,d), (c,f))

-- 7.5 Case expressions
myfunc x = case x + 1 == 1 of
           True -> "Awesome"
           False -> "wut"

checkStr x = case x of
             "my home" -> y
             "your home" -> y
             "yo mama" -> y
             _ -> False
  where y = True

-- 7.7 Guards
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100

-- 7.8 Function Composition
negateSumOfList :: [Integer] -> Integer
negateSumOfList = negate . sum

-- 7.11 Chapter Exercises
-- Let's write code
-- 1.
tensDigit :: Integral a => a -> a
tensDigit x = d
  where myDiv = flip divMod 100
        xLast = fst $ myDiv x
        d     = snd $ myDiv xLast

-- 3. 
g :: (a -> b) -> (a, c) -> (b, c)
g func (a, c) = (func a, c) 

