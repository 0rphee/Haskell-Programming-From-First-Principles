module RegisteredUser2 where
import Distribution.TestSuite (TestInstance(name))

newtype Username = Username String 
newtype AccountNumber = AccountNumber Integer 

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Unregistereduser"
printUser (RegisteredUser 
            (Username name) 
            (AccountNumber acctNum)) = 
    putStrLn $ name ++ " " ++ show acctNum

