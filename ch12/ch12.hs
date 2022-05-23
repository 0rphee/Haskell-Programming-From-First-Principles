{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
import Data.Char (isLetter, isAlpha)
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

isThe :: String -> Bool
isThe [] = False
isThe x = (==) "the" x

startsWithVow :: String -> Bool
startsWithVow [] = False
startsWithVow (x:_)
    | x `elem` "aeiou" = True
    | otherwise = False



countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel str = helper $ words str
    where helper :: [String] -> Integer
          helper [] = 0
          helper (x:xs) = case isThe x of
             False -> helper xs
             True -> case xs of
                 [] -> 0
                 _  -> case startsWithVow (head xs) of
                        False -> helper xs
                        True -> 1 + helper xs

-- Validate the word
newtype Word' = Word' String deriving (Eq, Show)
type NVows = Int
type NCons = Int

vowels :: [Char]
vowels ="aeiou"

consonants :: [Char]
consonants = "bcdfghjklmnpqrstvwxyz"

mkWord :: String -> Maybe Word'
mkWord [] = Nothing
mkWord str = case vows < cons of
    False -> Nothing
    True -> Just (Word' str)

    where helper :: String -> (NCons,NVows) -> (NCons,NVows)
          helper [] tup = tup
          helper (x:xs) tup@(vows,cons)
            | x `elem` consonants = helper xs (vows,cons+1)
            | x `elem` vowels     = helper xs (vows+1,cons)
            | otherwise           = helper xs tup
          (vows,cons) = helper str (0,0)

-- It's only Natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
    | 0 > x = Nothing
    | otherwise = Just (helper x)
    where helper 0 = Zero
          helper y = Succ (helper (y-1))

-- Small library for Maybe
-- 1. Simple boolean checks for Maybe values
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing x = not $ isJust x

-- 2. Maybe catamorphism
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just y) = f y

-- 3. In case you want to provide a fallback value
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

fromMaybe2 :: a -> Maybe a -> a
fromMaybe2 x = mayybee x id

-- 4. Converting between List and Maybe
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- 5. For when we want to drop the Nothing values from our list
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of
    Nothing -> catMaybes xs
    Just x -> x : catMaybes xs

-- 6. You'll see this called "sequence" later
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe maybes = helper maybes []
    where helper :: [Maybe a] -> [a] -> Maybe [a]
          helper [] carry = Just carry
          helper (Nothing:_) _ = Nothing
          helper (Just x:xs) carry = helper xs (carry++[x])

-- Small library for Either 
-- 1. Try to eventually arrive at a solution that uses foldr, 
-- even if earlier versions don’t use foldr.
lefts'1 :: [Either a b] -> [a]
lefts'1 [] = []
lefts'1 (Left a:xs) = a:lefts'1 xs
lefts'1 (Right _:xs) = lefts'1 xs

lefts' :: [Either a b] -> [a]
lefts' = foldr helper []
    where helper :: Either a b -> [a] -> [a]
          helper (Left a) next  = [a] ++ next -- without (++) it wont work
          helper (Right _) next = next
{- 
foldr helper [] [Left 1, Right 'a', Left 2]
Left 1 `helper` (foldr helper [] [Right 'a',Left 2])
Left 1 `helper` (Right 'a' `helper`(foldr helper [] [Left 2]))
Left 1 `helper` (Right 'a' `helper`(Left 2 `helper` []))
 -}

 -- 2. Same as the last one. Use foldr eventually
rights' :: [Either a b] -> [b]
rights' = foldr helper []
    where helper :: Either a b -> [b] -> [b]
          helper (Right b) next = [b] ++ next -- without (++) it wont work
          helper (Left _)  next = next

-- 3. partitionEithers'
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' eithers = helper eithers ([],[])
    where helper :: [Either a b] -> ([a],[b]) -> ([a],[b])
          helper [] carry = carry
          helper (Left a:xs) (lefs,righs) = helper xs (lefs++[a],righs)
          helper (Right a:xs) (lefs,righs) = helper xs (lefs,righs++[a])

-- 4. eitherMaybe'
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right a) = Just (f a)

-- 5. This is a general catamorphism for Either values
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

-- 6. Same as before, but use the either' function you just wrote
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' f (Right b) = Just (f b)

-- Unfolds 
-- 1. Write myIterate using direct recursion
myIterate :: (a -> a) -> a -> [a]
myIterate f starter = starter : myIterate f next
    where next = f starter

-- 2. Write myUnfoldr using direct recursion
myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f b = case f b of
    Nothing -> []
    Just (a,b) -> a : myUnfoldr f b

-- 3. Rewrite myIterate into betterIterate using myUnfoldr
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))

-- Finally something other than a list!

data BinaryTree a = Leaf 
                  | Node (BinaryTree a) a (BinaryTree a)
                    deriving (Eq,Ord,Show)

unfoldBTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfoldBTree f starter = case f starter of
    Nothing -> Leaf
    Just (l,n,r) -> Node (unfoldBTree f l) n (unfoldBTree f r) 

unfoldBTreeTest :: BinaryTree Int
unfoldBTreeTest = unfoldBTree (\a -> if a<4
                          then Just (a+1,a,a+2)
                          else Nothing) 0

-- 2. Make a tree builder
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldBTree (\a -> if a<n
                                 then Just (a+1,a,a+1)
                                 else Nothing) 0