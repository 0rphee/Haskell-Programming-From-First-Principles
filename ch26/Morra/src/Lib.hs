module Lib where

import qualified System.Console.ANSI as C
import System.IO ( hFlush, stdout )
import Data.Word ( Word8 )
import Control.Monad.Trans.State.Lazy ( StateT(StateT, runStateT) )
import System.Console.ANSI
import System.Random (randomR, initStdGen)
import Data.Char (isSpace)


newtype Abbreviation = Abb { getStr :: String }
  deriving Show

data WinCondition = Evens | Odds
                    deriving (Eq, Show)

data Player = P
  { pAbbreviation :: Abbreviation
  , pName         :: String
  , pWinCondition :: WinCondition
  , pScore        :: Word8
  }
              deriving (Show)

newtype GameState = GST (Player, Player)


computerRandomSelect :: IO Word8
computerRandomSelect = fst . randomR (1,2) <$> initStdGen

introName :: Player -> String
introName (P (Abb abb) name _ _) = "-- " <> abb <>  " is " <> name

showWCondn :: WinCondition -> String
showWCondn Evens = " is evens"
showWCondn Odds = " is odds"

introRole :: Player -> Player -> String
introRole (P _ name1 wcond1 _)
          (P _ name2 wcond2 _) =  "-- " <> name1
                              <> showWCondn wcond1 <>", "
                              <> name2 <> showWCondn wcond2

showIntro :: Player -> Player -> String
showIntro player1 player2 = introName player1 <> "\n"
                         <> introName player2 <> "\n"
                         <> introRole player1 player2

showComputerChoice :: IO Word8
showComputerChoice = do
  c <- computerRandomSelect
  putStrLn $ "C: " <> show c
  return c

showWinner :: Abbreviation -> String
showWinner (Abb abb) = "- " <> abb <> " wins"

putWinnerMsg :: String -> IO ()
putWinnerMsg str = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn str
  setSGR [Reset]

putErrorMsg :: Char -> IO ()
putErrorMsg incorrectChar = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn $ "ERROR: INCORRECT INPUT \"" <> shownChar <>"\" - please enter 1 or 2\n"
  setSGR [Reset]
  where shownChar = if incorrectChar == '\n'
                    then " "
                    else [incorrectChar]

clearPrevLine :: IO ()
clearPrevLine = C.cursorUpLine 1 >> C.clearLine

skipLine :: IO ()
skipLine = putStrLn ""

deletePlayerChoice :: IO ()
deletePlayerChoice = clearPrevLine

getPlayerSelection' :: Player -> IO Char
getPlayerSelection' player@(P (Abb abb) _ _ _) = do
  skipLine
  ch <- putStr (abb <> ": ") >> getChar <* skipLine
  sanitizedChar <- if ch == '\n'
                   then C.cursorUpLine 1 >> pure ' '
                   else if isSpace ch
                        then pure ' '
                        else pure ch
  if sanitizedChar == '1' || sanitizedChar == '2'
  then pure sanitizedChar
  else do
      clearPrevLine >> clearPrevLine >> clearPrevLine
      putErrorMsg sanitizedChar
      C.cursorUpLine 1
      hFlush stdout
      getPlayerSelection' player

getPlayerSelection :: Player -> IO Word8
getPlayerSelection p = do
  skipLine >> skipLine >> skipLine
  C.cursorUpLine 2
  hFlush stdout
  let str = getPlayerSelection' p
  read . (:[]) <$> str

increasePlayerScore :: Player -> Player
increasePlayerScore (P ab name wcondition score) = P ab name wcondition (score + 1)

updateGST :: GameState -> (Word8, Word8) -> (Abbreviation, GameState)
updateGST ( GST
            ( p1@(P _ _ wCondition1 _)
            , p2                     ) )
          ( choice1, choice2 )
  | odd (choice1 + choice2) = (pAbbreviation oddsWinner, GST (increasePlayerScore oddsWinner, evensWinner))
  | otherwise = (pAbbreviation evensWinner, GST (oddsWinner, increasePlayerScore evensWinner))
  where (oddsWinner, evensWinner) = case wCondition1 of
                                    Odds -> (p1, p2)
                                    Evens -> (p2, p1)

stateComp :: GameState -> IO (Abbreviation {- abb of the winner -}, GameState)
stateComp initGST@(GST (p1, p2 {- Computer -})) = do
  putStrLn $ showIntro p1 p2
  playerSel <- getPlayerSelection p1
  C.cursorUpLine 3 >> C.clearLine >> C.cursorDownLine 3
  computSel <- showComputerChoice
  let newGST = updateGST initGST (playerSel, computSel)
  pure newGST

putP1ScoreAgain :: Player -> Word8 -> IO ()
putP1ScoreAgain player playerSelect= do
  C.cursorUpLine 3 >> C.clearLine >> C.cursorDownLine 1 >> C.clearLine -- clear Error line and go back down 
  putStr ((getStr . pAbbreviation ) player <> ": " <> show playerSelect)
  C.cursorDownLine 2 -- go back to original position

statePlayers :: GameState -> IO (Abbreviation {- abb of the winner -}, GameState)
statePlayers initGST@(GST (p1, p2 )) = do
  putStrLn $ showIntro p1 p2
  playerSel1 <- getPlayerSelection p1
  clearPrevLine >> clearPrevLine >> clearPrevLine -- clear printed lines during selection of P1
  playerSel2 <- getPlayerSelection p2
  putP1ScoreAgain p1 playerSel1
  let newGST = updateGST initGST (playerSel1, playerSel2)
  pure newGST

checkIfWinner :: GameState -> Either GameState Player
checkIfWinner gst@(GST ( p1@(P _ _ _ s1)
                       , p2@(P _ _ _ s2)))
  | s1 > s2 && s1 >= 10 = Right p1
  | s1 < s2 && s2 >= 10 = Right p2
  | otherwise = Left gst

rightF :: Player -> IO ()
rightF (P _ name _ _) = putStrLn $ name <> " wins the game!"

runComputerGame :: GameState -> IO () -- TODO generalize function 
runComputerGame initGST = do
  (abb, gst) <- runStateT (StateT stateComp) initGST
  putWinnerMsg $ showWinner abb
  skipLine
  case checkIfWinner gst of
    Left gst' -> runComputerGame gst'
    Right p -> rightF p


runTwoPlayerGame :: GameState -> IO ()
runTwoPlayerGame initGST = do
  (abb, gst) <- runStateT (StateT statePlayers) initGST
  putWinnerMsg $ showWinner abb
  skipLine
  case checkIfWinner gst of
    Left gst' -> runTwoPlayerGame gst'
    Right p -> rightF p


