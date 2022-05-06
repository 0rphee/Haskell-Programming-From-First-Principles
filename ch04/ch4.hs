-- Exercises: Mood Swing
data Mood = Blah | Woot deriving Show
changeMood Blah = Woot
changeMood Woot = Blah

-- 4.9 Chapter Exercises
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- 8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == x'
    where x' = reverse x

-- 9
myAbs :: Integer -> Integer
myAbs x = if x >= 0
          then x
          else (-x)

-- 10
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

-- Correcting Sintax
-- 1
x = (+)
func xs = x w 1
     where w = length xs

-- 2 Identity function
idFunc x = x

-- 3
anotherFunc (a, b) = a

