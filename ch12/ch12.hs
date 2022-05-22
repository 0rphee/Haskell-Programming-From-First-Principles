import Data.Char (isLetter)
-- Chapper 12: Signaling adversity

-- 12.2
-- Smart constructors for datatypes
type Name = String
type Age = Integer
data Person = Person Name Age deriving Show

{- First version mkPerson
mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
    | name /= "" && age >= 0 = Just $ Person name age
    | otherwise = Nothing
 -}
-- 12.3 Bleating either
-- data Either a b = Left a | Right b
data PersonInvalid = NameEmpty | AgeTooLow
    deriving (Eq,Show)
type ValidatePerson a = Either [PersonInvalid] a

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age >= 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | otherwise = Left AgeTooLow

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
    True -> Right age
    False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
    True -> Right name
    False -> Left [NameEmpty]

mkPerson2 :: Name -> Age -> ValidatePerson Person
mkPerson2 name age = helper (nameOkay name) (ageOkay age)
    where helper :: ValidatePerson Name -> ValidatePerson Age
                 -> ValidatePerson Person
          helper (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
          helper (Left badName) (Left badAge) =  Left (badName ++ badAge)
          helper (Left badName) _ = Left badName
          helper _ (Left badAge) = Left badAge

-- 12.4 Kinds, a thousand stars in your types
-- String processing

replaceThe :: String -> String
replaceThe str = unwords (map replaceThe wrds)
    where wrds = words str :: [String]
          replaceThe x = if isThe x
                         then "a"
                         else x
isThe = (==) "the"

-- a mess
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel str 
    | isThe x && isNextVow y = 1 + countTheBeforeVowel remainder
    | otherwise = 0 + countTheBeforeVowel remainder
    where wrds = words str
          (x:y) = take 2 wrds
          isNextVow [] = False
          isNextVow (z:_) = isLetter z
          remainder = concat (drop 2 wrds)