-- Algebraic datatypes
-- newtype
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-} to derive all typeclasses
newtype Goats = Goats Int deriving (Eq, Show)

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany Goats where
    tooMany (Goats n) = n >43



data OperatingSystem =
       GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill | Mac
    | Windows
    deriving (Eq, Show)
data ProgLang =
       Haskell
    | Agda
    | Idris
    | PureScript deriving (Eq, Show)
data Programmer =
    Programmer { os :: OperatingSystem
                , lang :: ProgLang }
                deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux,
                        OpenBSDPlusNevermindJustBSDStill,
                        Mac,
                        Windows ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]


newtype Name = Name String deriving (Show, Eq)
newtype Acres   = Acres Int deriving (Show, Eq)
-- FarmerType is a Sum
data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving (Show, Eq)
-- Farmer is a plain ole product of -- Name, Acres, and FarmerType
data Farmer =
  Farmer Name Acres FarmerType
  deriving (Show, Eq)

isSthFarmer :: FarmerType -> Farmer -> Bool
isSthFarmer toMatch (Farmer _ _ typeFarmer) = typeFarmer == toMatch

isDairyFarmer :: Farmer -> Bool
isDairyFarmer = isSthFarmer DairyFarmer

bookIsDairyFarmer :: Farmer -> Bool
bookIsDairyFarmer (Farmer _ _ DairyFarmer) = True
bookIsDairyFarmer _                        = False 

data FarmerRec = FarmerRec {name :: Name,
                            acres :: Acres,
                            farmerType :: FarmerType}

isDairyFarmerRec farmer = case farmerType farmer of
                          DairyFarmer -> True 
                          _           -> False

-- 11.14 Function type is exponential
-- given a function a -> b there will be b^a inhabitants
--                  a -> b -> c will have  (c^b)^a or c^(b*a)

data Quantum = Yes | No | Both deriving (Eq,Show)

-- 3 + 3
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes
quantSum2 :: Either Quantum Quantum
quantSum2 = Right No
quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both
quantSum4 :: Either Quantum Quantum 
quantSum4 = Left Yes
quantSum5 :: Either Quantum Quantum 
quantSum5 = Left No
quantSum6 :: Either Quantum Quantum 
quantSum6 = Left Both

-- 3 * 3
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)
quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)
quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)
quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)
quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)
quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)
quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)
quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)
quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)

-- 3 ^ 3
quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes  = Yes
quantFlip1 No   = Yes
quantFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes  = Yes
quantFlip2 No   = Yes
quantFlip2 Both = No

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes  = Yes
quantFlip3 No   = Yes
quantFlip3 Both = Both

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes  = Yes
quantFlip4 No   = No
quantFlip4 Both = Yes

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes  = Yes
quantFlip5 No   = Both
quantFlip5 Both = Yes

quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes  = No
quantFlip6 No   = Yes
quantFlip6 Both = Yes

quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes  = Both
quantFlip7 No   = Yes
quantFlip7 Both = Yes

quantFlip8 :: Quantum -> Quantum
quantFlip8 Yes  = Both
quantFlip8 No   = Yes
quantFlip8 Both = No

quantFlip9 :: Quantum -> Quantum
quantFlip9 Yes  = Both
quantFlip9 No   = No
quantFlip9 Both = No

quantFlip10 :: Quantum -> Quantum
quantFlip10 Yes  = Both
quantFlip10 No   = No
quantFlip10 Both = Both
-- and more

convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes  = True
convert3 No   = False
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes  = False
convert4 No   = False
convert4 Both = False

-- 11.15 Higher-kinded datatypes
-- 11.16 List are polymorphic
data List a = Nil | Cons a (List a)
instance (Show a) => Show (List a) where
    show Nil = "//"
    show (Cons a b) = show a ++ " | " ++ show b

-- 11.17 Binary Tree
data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a) 
                  deriving (Eq,Ord)
instance (Show a) => Show (BinaryTree a) where
    show Leaf = " "
    show (Node a b c) = "<" ++ show a ++ "("++show b++")"++show c++">"