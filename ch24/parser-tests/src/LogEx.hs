module LogEx where

import Control.Monad (void)
import Control.Applicative
import Text.Trifecta
import Text.Read (readMaybe)
import Data.Maybe (fromJust, isNothing)

comment = "-- comment"
day = "# 2025-02-05"
hour = "08:00"
activity = "09:00 Sanitizing moisture collector"
dayWcomment = day ++ comment
activityWcomment = activity ++ comment

testAll = do
  print $ parseString skipComment mempty comment
  print $ parseString parseDate mempty day
  print $ parseString parseHour mempty hour
  print $ parseString parseActivity mempty activity


type Year = Integer
type Month = Integer
type Day = Integer

data Date = Date Year Month Day 
            deriving (Eq, Show)


type Hour = Integer 
type Minute = Integer

data DayTime = DayTime Hour Minute
               deriving (Eq, Show)


type ActName = String
data Activity = Activity ActName DayTime
               deriving (Eq, Show)
skipComment :: Parser ()
skipComment = void $ string "--"

parseDate :: Parser Date
parseDate = do 
  year <- char '#' >> spaces >> integer
  month <- char '-' >> integer
  day <- char '-' >> integer
  return $ Date year month day

parseHour :: Parser DayTime
parseHour = do
  hour <- integer
  minute <- char ':' >> integer
  return (DayTime hour minute)

-- still doesnt work
parseActivity :: Parser Activity
parseActivity = do
  hour <- parseHour
  activity <- spaces >> manyTill stringLiteral (noneOf "\n" )
  return (Activity (concat activity) hour)




