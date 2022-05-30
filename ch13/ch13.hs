-- Chapter 13: Building projects
type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
         "Name was: " ++ show name ++
         " Age was: " ++ show age

askSth :: String -> IO String
askSth prompt = do
    putStrLn prompt
    getLine

reportRes :: Either PersonInvalid Person -> IO ()
reportRes (Left invalid) = print invalid
reportRes (Right valid) = putStrLn ("Yay! Succesfully got a person: " <> show valid)

gimmePerson :: IO ()
gimmePerson = do
    name <- askSth "Input your name"
    age <- askSth "Input your age"
    let person = mkPerson name (read age)
    reportRes person

main :: IO ()
main = gimmePerson