module Reverse where
rvrs :: String -> String
rvrs str = awes ++ is ++ cur
     where awes = take 7 (drop 9 str)
           is = take 4 (drop 5 str)
           cur = take 5 str

main :: IO()
main = print $ rvrs "Curry is awesome"