module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1' 

one' :: Parser b
one' = one >> stop

oneTwo :: Parser Char
oneTwo = one >> char '2'

oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

oneTwoThree :: Parser Char
oneTwoThree = oneTwo >> char '3' 

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParseS :: Parser String -> IO ()
testParseS p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':" 
  testParse one' 
  pNL "oneTwo:" 
  testParse oneTwo 
  pNL "oneTwo':" 
  testParse oneTwo'
  pNL "myParser:"
  testParseS myParser
  pNL "one >> eof"
  testEOF (one >> eof)
  pNL "oneTwo >> eof"
  testEOF (oneTwo >> eof)
  pNL "myParser >> eof"
  testEOF (myParser >> eof)
  pNL "string '1', '12', '123'"
  testParse (choice [ oneTwoThree
                     , oneTwo
                     , one
                     , stop])
  testParseS (choice [ three
                     , two
                     , on
                     , stop])


-- Exercises: Parsing Practice
testEOF :: Parser () -> IO ()
testEOF p = print $ parseString p mempty "123"

myParser :: Parser String
myParser = string "123"

three :: Parser String
three = string "123"

two :: Parser String
two = string "12"

on :: Parser String 
on = string "1"


