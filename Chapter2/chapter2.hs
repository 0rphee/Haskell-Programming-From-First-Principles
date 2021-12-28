sayHello :: String -> IO ()
sayHello x = 
         putStrLn ("Hello, " ++ x ++ "!")

-- Exercises: Comprehension check
half x = x/2
square x = x**2

piAndSquareProd n = pi * (n**2)

-- Exercises: A Head Code
var1 = x
     where x = 5

var2 = x
     where x = 5**2

var3 = x * y
     where x = 5
           y = 6
var4 = x + 3
     where x = 3
           y = 1000

var5 = z/x + y 
     where x = 7
           y = negate x
           z = y * 10