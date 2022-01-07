data DayOfWeek =
    Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Ord, Show)
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
newtype TisAnInteger = TisAn Integer
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

-- Working with Show
data Mood = Blah deriving Show

-- 6.14 Chapter Exercises
-- Does it typechek?
-- 4.
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
                deriving (Eq, Show)
s1 = Sentence "dogs" "drool" -- turns into a "function" of sorts
s2 = Sentence "Julie" "loves" "dogs" -- an actual value

-- Given a datatype declaration, what can we do?
newtype Rocks = Rocks String deriving (Eq, Show)
newtype Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)
phew = Papu (Rocks "chases") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- Type-Kwon-Do Two: Electric Typealoo
-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk arg mag tag = arg mag == tag

--2. 
arith :: Num b =>
        (a -> b) -> Integer -> a -> b
arith tup int third = tup third + fromInteger int