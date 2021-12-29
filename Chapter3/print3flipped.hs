module Print3Flipped where

myGreeting :: [Char]
myGreeting = (++) "hello" " world"

hello :: [Char]
hello = "hello"

world :: [Char]
world = "world!"

main = do
       putStrLn myGreeting
       putStrLn secondGreeting
       where secondGreeting = 
                (++) hello ((++) " " world)