module Exercises.IPv where

import Data.Word
import Data.Bits
import Text.Trifecta
import Data.Char

data IPAddress = 
  IPAddress Word32
  deriving (Eq, Ord, Show)

testString = "192.0.2.235 "
testWordResult = IPAddress 3221226219

type PackedAdress = (Word32,Word32,Word32,Word32)

testAddress :: PackedAdress
testAddress = (192,0,2,235)

testIP :: Bool
testIP = toIPAddress testAddress == IPAddress 3221226219

toIPAddress :: PackedAdress -> IPAddress
toIPAddress (a, b, c, d) =
  IPAddress (a' + b' + c' + d')
  where a' = shift a 24
        b' = shift b 16
        c' = shift c 8
        d' = d

parseAddress' :: Parser PackedAdress
parseAddress' = do
  a <- integer
  b <- char '.' >> integer
  c <- char '.' >> integer
  d <- char '.' >> integer
  return ( fromIntegral a
         , fromIntegral b
         , fromIntegral c
         , fromIntegral d
         ) <?> "An IPv4 Address (x.x.x.x)"
        
       

parseAddress :: Parser IPAddress
parseAddress = toIPAddress <$> parseAddress'

runPAddress :: Result IPAddress 
runPAddress = runParser parseAddress mempty testString 

testResult :: Result Bool
testResult = (==) <$> runPAddress <*> return testWordResult 

data IPAddress6 = IPAddress6 Word64 Word64
                  deriving (Eq, Ord, Show)
                  
parseHex :: Parser String
parseHex = some hexDigit

hexToWord :: String -> Word
hexToWord s = helper (reverse s) 0
  where helper [] carry = carry
        helper (x:xs) carry = 
          helper xs (carry + a) 
          where a = case toLower x of
                    '1' -> 1
                    '2' -> 2
                    '3' -> 3
                    '4' -> 4
                    '5' -> 5
                    '6' -> 6
                    '7' -> 7
                    '8' -> 8
                    '9' -> 9
                    'a' -> 10
                    'b' -> 11
                    'c' -> 12
                    'd' -> 13
                    'e' -> 14
                    'f' -> 15
                    _   -> 0 





