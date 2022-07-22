module Exercises.Exercises where

import Control.Applicative
import Text.Trifecta
import Text.Read (readMaybe)
import Data.Maybe (fromJust, isNothing)


data NumberOrString = NOSS String
                    | NOSI Integer
                    deriving (Eq, Show)
type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer 
  Major Minor Patch Release Metadata
  deriving (Show, Eq)
instance Ord SemVer where
  compare (SemVer ma1 mi1 pa1 _ _) (SemVer ma2 mi2 pa2 _ _) 
    | ma /= EQ = ma
    | mi /= EQ = mi
    | pa /= EQ = pa
    | otherwise = EQ
    where ma = compare ma1 ma2
          mi = compare mi1 mi2
          pa = compare pa1 pa2

getNOS :: String -> NumberOrString
getNOS a = case readMaybe a of
  Nothing -> NOSS a
  Just b  -> NOSI b

parseNOS :: Parser NumberOrString
parseNOS = do
  val <- some alphaNum 
  _   <- many $ char '.'
  return $ getNOS val

parseSemVer :: Parser SemVer
parseSemVer = do
  ma <- integer
  mi <- char '.' >> integer 
  pa <- char '.' >> integer
  re <- many (char '-') >> many parseNOS
  me <- many parseNOS
  return $ SemVer ma mi pa re me

test1 = parseString parseSemVer mempty "2.1.1"
-- Success (SemVer 2 1 1 [] [])
test2 = parseString parseSemVer mempty "1.0.0-x.7.z.92"
-- Success (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])
test3 = SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []

-- 2.
parseDigit :: Parser Char
parseDigit = oneOf "1234567890" <?> "a digit between 0 and 9"

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit <?> "integer"

-- 3.
base10Integer' :: Parser Integer
base10Integer' = do
  sign <- many $ char '-'
  val <- base10Integer
  case sign of
    [] -> return val 
    _ -> return $ negate val

-- 4. 
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber 
                     NumberingPlanArea
                     Exchange
                     LineNumber
                  deriving (Eq, Show) 

intSection :: Parser Int
intSection = fromInteger <$> (many (char '-') >> integer)

parsePhoneF1 :: Parser PhoneNumber
parsePhoneF1 = do
  npa <- intSection
  ex  <- intSection
  ln  <- intSection
  return (PhoneNumber npa ex ln)

parenSection :: Parser Int
parenSection = fromInteger <$> parens integer

parenSection' :: Parser Int
parenSection' = fromInteger <$> 
  (char '(' *> integer <* char ')')

parsePhoneF2 :: Parser PhoneNumber
parsePhoneF2 = do
  npa <- parenSection
  ex  <- spaces >> intSection
  ln  <- intSection
  return (PhoneNumber npa ex ln)
  
parsePhoneF3 :: Parser PhoneNumber
parsePhoneF3 = intSection >> parsePhoneF1

parsePhoneF4 :: Parser PhoneNumber
parsePhoneF4 = do
  whole <- show <$> integer
  let npa = readMaybe $ take 3 whole
  let ex  = readMaybe $ drop 3 $ take 6 whole
  let ln  = readMaybe $ drop 6 whole
  case isNothing npa || isNothing ex || isNothing ln of
    True -> fail "expected plain number"
    _       -> return (PhoneNumber (fromJust npa) (fromJust ex) (fromJust ln))
parsePhone :: Parser PhoneNumber
parsePhone = try parsePhoneF3
         <|> try parsePhoneF1
         <|> try parsePhoneF4
         <|> try parsePhoneF2

phone1 = parseString parsePhone mempty "123-456-7890"
phone2 = parseString parsePhone mempty "(123) 456-7890"
phone3 = parseString parsePhone mempty "1-123-456-7890"
phone4 = parseString parsePhone mempty "1234567890"


