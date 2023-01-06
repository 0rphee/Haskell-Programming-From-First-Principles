
x = undefined
y = "blah"

main = do
  print (snd $ x `seq` (x, y))
