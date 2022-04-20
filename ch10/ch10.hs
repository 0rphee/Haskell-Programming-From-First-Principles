import Data.Time
--Folds
myfold :: [String] -> String
myfold = foldl (\prev next -> prev ++ take 3 next) ""

-- Exercises: Database Processingx

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [DbDate (UTCTime
                        (fromGregorian 1911 5 1)
                        (secondsToDiffTime 34123))
               , DbNumber 9001
               , DbString "Hello, World"
               , DbDate (UTCTime (fromGregorian 1921 5 1)
                                 (secondsToDiffTime 34123))
               ]

filt :: (t -> [a]) -> [a] -> t -> [a]
filt f p n = p ++ f n

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldl filtDate []
    where filtDate = filt (\x -> case x of
                                    DbDate y -> [y]
                                    _        -> [] )


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldl filtNumber []
    where filtNumber = filt (\x -> case x of
                                    DbNumber y -> [y]
                                    _          -> [] )


filterDbString :: [DatabaseItem] -> [String]
filterDbString = foldl filtString []
    where filtString = filt (\x -> case x of
                                    DbString y -> [y]
                                    _          -> [] )

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent dbItems = foldl1 max [] times
    where times = filterDbDate dbItems
          baseCase = UTCTime
                        (fromGregorian 1911 5 1)
                        (secondsToDiffTime 34123)

