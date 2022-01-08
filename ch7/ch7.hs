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

