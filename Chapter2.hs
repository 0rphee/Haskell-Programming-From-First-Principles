sayHello :: String -> IO ()
sayHello x = 
    putStrLn ("Hello, " ++ x ++ "!")

-- Exercises: Comprehension check
half x = x/2
square x = x**2

piAndSquareProd :: Floating a => a -> a
piAndSquareProd n = pi * (n**2)
