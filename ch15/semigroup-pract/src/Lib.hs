module Lib where

import Test.QuickCheck
import Control.Arrow (ArrowChoice(right))

data Trivial = Trivial deriving (Eq,Show)

instance Semigroup Trivial where
  a <> b = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

trivialCheck :: IO ()
trivialCheck = quickCheck (semigroupAssoc :: TrivAssoc)

-- 2. 
newtype Identity a = Identity a deriving (Eq,Show)
instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdenAssoc = Identity Trivial -> Identity Trivial
               -> Identity Trivial -> Bool

identityCheck :: IO ()
identityCheck = quickCheck (semigroupAssoc :: IdenAssoc)

-- 3.
data Two a b = Two a b deriving (Eq,Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a<>c) (b<>d)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoIntChar = Two [Char] Trivial
type TwoAssoc = TwoIntChar -> TwoIntChar -> TwoIntChar -> Bool

twoCheck :: IO ()
twoCheck = quickCheck (semigroupAssoc :: TwoAssoc)

-- 6. 
newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _                             = BoolConj False
instance Monoid BoolConj where
  mempty = BoolConj True

-- 7.
newtype BoolDisj = BoolDisj Bool deriving (Eq,Show)
instance Semigroup BoolDisj where
  (BoolDisj True) <> (BoolDisj _) = BoolDisj True
  (BoolDisj _) <> (BoolDisj True) = BoolDisj True
  (BoolDisj _) <> (BoolDisj _) = BoolDisj False
instance Monoid BoolDisj where
  mempty = BoolDisj False

-- 8. 
data Or a b = Fst a | Snd b deriving (Show,Eq)
instance Semigroup (Or a b) where
  (Snd a) <> _ = Snd a
  _ <> (Snd a) = Snd a
  _ <> (Fst a) = Fst a

-- 9.
newtype Combine a b = Combine {unCombine :: a -> b}
instance Show (Combine a b) where
  show _ = "Combine"
instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine left) <> (Combine right) = Combine (\x -> left x <> right x)

-- 10. 
newtype Comp a = Comp {unComp :: a -> a}
instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

-- 11. 
data Validation a b = Failuree a | Successs b deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
  s@(Successs _) <> _ = s
  _ <> s@(Successs _) = s
  (Failuree a) <> (Failuree b) = Failuree (a<>b) 

main = do
  let failure :: String -> Validation String Int
      failure = Failuree
      success :: Int -> Validation String Int
      success = Successs
  print $ success 1 <> failure "blah" 
  print $ failure "woot" <> failure "blah" 
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

-- Monoids
-- 8.
newtype Mem s a = Mem {runMem :: s -> (a,s)}
instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) =
    Mem $ \s ->
      let (a', s') = g s
          (a'', s'') = f s'
       in (a'' <> a', s'')
instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)

f' = Mem $ \s -> ("hi", s+1)

mMain = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0 
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int)) 
  print $ rmleft == runMem f' 0 
  print $ rmright == runMem f' 0