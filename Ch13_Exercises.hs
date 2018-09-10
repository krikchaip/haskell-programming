module Ch13_Exercises where

import Control.Monad
import Data.Char (toLower, isAlpha)
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let definition = filter isAlpha . map toLower $ line1
  if definition == reverse definition
    then putStrLn "It's a palindrome!"
    else do
      putStrLn "Nope!"
      exitSuccess

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | age <= 0              = Left AgeTooLow
  | otherwise             = Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  name <- prompt "name: "
  age  <- prompt "age: "
  let person = mkPerson name (read age :: Age)
  case person of
    Right p -> putStrLn $ "Yay! Successfully got a person: " ++ show p
    Left  e -> print e
  where prompt s = putStr s >> getLine
