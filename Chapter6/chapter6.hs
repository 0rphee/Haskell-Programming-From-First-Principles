data DayOfWeek =
    Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving Show 
instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _     = False

data Date =
    Date DayOfWeek Int
    deriving Show
instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') =
        weekday == weekday' && dayOfMonth == dayOfMonth'

-- Exercsices: Eq Instances
-- 1.
data TisAnInteger = TisAn Integer 
    deriving Show
instance Eq TisAnInteger where
    (==) (TisAn int) (TisAn int') = int == int'

-- 2.
data TwoIntegers = Two Integer Integer
                   deriving Show
instance Eq TwoIntegers where
    (==) (Two int1 int2) (Two int1' int2') =
         int1 == int1' && int2 == int2'

-- 3.
data StringOrInt = TisAnInt Int 
                 | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt int) (TisAnInt int')     = int == int'
    (==) (TisAString str) (TisAString str') = str == str'
    (==) _ _ = False

-- 4.
data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair a1 a2) (Pair a1' a2') 
         = a1 == a1' && a2 == a2'

-- 5.
data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') =
         a == a' && b == b'

-- 6.
data Which a = ThisOne a 
             | ThatOne a
instance (Eq a) => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) (ThatOne a) (ThisOne a') = a == a'
    (==) (ThisOne a) (ThatOne a') = a == a'

-- 7.
data EitherOr a b = Hello a
                  | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye b) (Goodbye b') = b == b'
    (==) _ _ = False