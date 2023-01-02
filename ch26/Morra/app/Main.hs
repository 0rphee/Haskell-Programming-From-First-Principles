module Main (main) where

import Lib
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  
  putStrLn "\nPress ENTER in order to play against another human\nPress any other key to play against the computer" 
  char <- getChar <* putStrLn "\n-----------------------\n"
  hFlush stdout
  if char == '\n' 
  then runTwoPlayerGame twoPlayerGST
  else runComputerGame initComputerGST
  -- runTwoPlayerGame twoPlayerGST

initComputerGST :: GameState
initComputerGST = GST ( P (Abb "P") "Player" Odds 0
                      , P (Abb "C") "Computer" Evens 0 )

twoPlayerGST :: GameState
twoPlayerGST = GST ( P (Abb "P1") "Player 1" Odds 0
                   , P (Abb "P2") "Player 2" Evens 0 )

