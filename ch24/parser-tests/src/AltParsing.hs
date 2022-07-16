{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ
import Text.Fractions

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = a ++ b

parseNos :: Parser NumberOrString
parseNos = skipMany (oneOf "\n")
        >> (Left <$> integer)
       <|> (Right <$> some letter) 

parseNos' :: Parser NumberOrString
parseNos' = do
  skipMany (oneOf "\n")
  v <-     (Left <$> integer)
       <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

eitherOr :: String
eitherOr = [r|
123
abc
456
def|]

fracInt :: String
fracInt = "123"


mainAlt :: IO ()
mainAlt = do
  let p f i = parseString f mempty i
  print $ p (some letter) a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c
  print $ p (some parseNos) c
  print $ p parseNos eitherOr
  print $ p (some parseNos') eitherOr
  print $ p trr "a1/a12a" 

-- Exercise: Try Try
type FracOrInt = Either Integer Rational
trr = do
  skipMany (oneOf "\n")
  v <-    (Right <$> try virtousFraction)
      <|> (Left <$> decimal)
  skipMany (oneOf "\n")
  return v


