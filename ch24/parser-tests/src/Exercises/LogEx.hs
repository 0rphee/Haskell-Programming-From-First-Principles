module Exercises.LogEx where

import Control.Monad (void)
import Control.Applicative
import Text.Trifecta
import Text.Read (readMaybe)
import Data.Maybe (fromJust, isNothing)

comment = "   -- comment"
day = "# 2025-02-05"
hour = "08:00"
activity = "09:00 Sanitizing moisture collector"
dayWcomment = day ++ comment
activityWcomment = activity ++ comment

tryParse parser text annotations parName = 
  putStrLn $ concat [parName,"\n",
                     annotations,"\n",
                     show (parseString parser mempty text),
                     "\n"
                    ]
           

testAll = do
  tryParse skipCommentLine comment comment "skipCommentLine"
  tryParse parseDate day day "parseDate"
  tryParse parseHour hour hour "parseHour"
  tryParse parseActivity activity activity "parseActivity"
  tryParse parseActivity activityWcomment activityWcomment "parseActivity"

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
skipCommentLine :: Parser ()
skipCommentLine = do
  try spaces
  skipDashes <|> eof

skipDashes :: Parser ()
skipDashes = void (string "--")

skipInlineComment :: Parser ()
skipInlineComment = try (someSpace >> skipDashes)
                <|> skipDashes

parseText :: Parser Char
parseText = noneOf "\n"

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

parseActivity :: Parser Activity
parseActivity = do
  hour <- parseHour <* spaces
  activity <- try 
                (manyTill parseText skipInlineComment
                <|> many parseText)
  return (Activity activity hour)
--parse dayWcomment
-- parse activityWcomment


