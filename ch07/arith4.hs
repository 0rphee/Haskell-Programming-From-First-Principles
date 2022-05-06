module Airth4 where

roundTrip :: (Read a, Show a) => a -> a
roundTrip a = read (show a)

roundT :: (Read a, Show a) => a -> a
roundT = read . show 


main = do
    print (roundT 4)
    print (id 4)