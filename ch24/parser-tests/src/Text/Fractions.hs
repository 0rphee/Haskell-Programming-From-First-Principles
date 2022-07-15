{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtousFraction :: Parser Rational
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


