
import Data.Char

caesarCycle :: Integral a => Char -> a -> Char
caesarCycle lttr 0 = lttr
caesarCycle lttr n
    | lttr == 'z' && mod26 > 0 = caesarCycle 'a' (mod26-1)
    | otherwise = caesarCycle (func lttr) (mod26-1 )
    where mod26 = mod n 26
          func = if mod26 > 13
                 then pred
                 else succ

caesar2 :: Integral a => Char -> a -> Char
caesar2 lttr 0 = lttr
caesar2 lttr n
    | lttr == 'z' && not isNegative = caesarCycle 'a' operation
    | lttr == 'a' && isNegative = caesarCycle 'z' operation
    | otherwise = caesarCycle (func lttr) operation
    where dist = realDist n
          isNegative = dist < 0
          operation = if isNegative
                      then dist + 1
                      else dist - 1
          func = if isNegative
                 then pred
                 else succ

realDist :: Integral a => a -> a
realDist n
    | posMod <= abs negMod = posMod
    | otherwise = negMod
    where posMod = mod n 26
          negMod = mod n (-26)

caesar3 :: Char -> Int -> Char 
caesar3 letter offset 
    | realOffset == 0 = letter
    | otherwise = toEnum (originInt + realOffset)
    where realOffset = mod offset 26
          originInt = fromEnum letter
