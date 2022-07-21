{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork:: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m)
              => m Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtousFraction :: (Monad m, MonadFail m, TokenParsing m)
                => m Rational
virtousFraction  = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of 
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)
  
intEof :: Parser Integer
intEof = do
  n <- integer
  eof
  return n

testVirtuous :: IO ()
testVirtuous = do
  let virtousFraction' = parseString virtousFraction mempty
  print $ virtousFraction' badFraction
  print $ virtousFraction' alsoBad
  print $ virtousFraction' shouldWork
  print $ virtousFraction' shouldAlsoWork
 
mainF :: IO ()
mainF = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction
  
-- Exercise: Unit of Success
testIntEof :: String -> IO ()
testIntEof s = do
  print $ parseString intEof mempty s 

-- 24.9 Polymorphic parsers
mainAtto :: IO ()
mainAtto = do
  let attoP = parseOnly virtousFraction
  print $ attoP badFraction
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork
  print $ attoP alsoBad





