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

