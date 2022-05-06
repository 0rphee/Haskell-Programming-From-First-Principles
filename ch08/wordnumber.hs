module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> [Char] 
digitToWord n = case n of
                    1 -> "one"
                    2 -> "two"
                    3 -> "three"
                    4 -> "four"
                    5 -> "five"
                    6 -> "six"
                    7 -> "seven"
                    8 -> "eight"
                    9 -> "nine"
                    _ -> "zero"

digits :: Int -> [Int]
digits int = map read strList
    where charList = show int
          strList = map (:[]) charList

wordNumber :: Int -> [Char]
wordNumber n = concat hyphenatedWordList
    where digitList = digits n
          wordList = map digitToWord digitList
          hyphenatedWordList = intersperse "-" wordList