{-# LANGUAGE InstanceSigs #-}

module SuccessAndFailure.Validation7 where

-- Sort imports
-- Qualify imports
import qualified Data.Char       as Char
import qualified Data.List       as List
import qualified Data.Validation as Validation

main :: IO ()
main = do
    userName <- prompt "Please enter your username." >> (Username <$> getLine)
    pass <- prompt "Please enter your password." >> (Password <$> getLine)
    print (validateUsername userName)
    print (validatePassword pass)

test :: IO ()
test = printTest $ do
           eq 1 (checkPasswordLength "") (Right $ Password "")
           eq 2 (checkPasswordLength "julielovesbooks") (Right $ Password "julielovesbooks")

-- VALIDATION FUNCTIONS
makeUser :: Username -> Password -> Either Error User
makeUser userName passWord = User <$> validateUsername userName <*> validatePassword passWord

validatePassword :: Password -> Either Error Password
validatePassword (Password pass) = do
    cleaned    <- cleanWhitespace pass
    withAlphas <- requireAlpha cleaned
    checkPasswordLength withAlphas

validateUsername :: Username -> Either Error Username
validateUsername (Username userName) = cleanWhitespace userName >>= requireAlpha >>= checkUsernameLength

makeError :: String -> Error
makeError s = Error [s]

-- TYPES
data User = User Username Password
newtype Error = Error [String] deriving (Show, Eq)
newtype Password = Password String deriving (Show, Eq)
newtype Username = Username String deriving Show

instance Semigroup Error where
    (<>) :: Error -> Error -> Error
    (<>) (Error xs) (Error ys) = Error (xs <> ys)


-- VALIDATION RULES

checkPasswordLength :: String -> Either Error Password
checkPasswordLength s = case (length s > 20 || length s < 10) of
                               True  -> Left (makeError "Your password must be between 10 and 20 characters.")
                               False -> Right (Password s)

requireAlpha :: String -> Either Error String
requireAlpha s = case (List.all Char.isAlphaNum s) of
                     False -> Left (makeError "Your password cannot contain whitespace or special characters.")
                     True  -> Right s

cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left (makeError "Your password cannot be empty.")
cleanWhitespace s@(x:xs) = case (Char.isSpace x) of
                             True  -> cleanWhitespace xs
                             False -> Right s

checkUsernameLength :: String -> Either Error Username
checkUsernameLength name = case (length name > 15) of
                               True -> Left (makeError "Username cannot be longer than 15 characters.")
                               False -> Right (Username name)

checkLength :: Int -> String -> Either Error String
checkLength maxLength s = case (length s > maxLength) of
                              False -> Right s
                              True -> Left $ makeError msg
    where msg = List.intercalate " " [ "String"
                                     , s
                                     , "cannot be longer than"
                                     , show maxLength
                                     , "characters."]

prompt :: String -> IO ()
prompt s = putStr $ s <> "\n> "

getPass :: IO String
getPass = getLine

reverseLine :: IO ()
reverseLine = do
  line <- getLine
  print (reverse line)

-- TESTING FRAMEWORK
printTest :: Either String () -> IO ()
printTest e = case e of
                  Left s  -> putStrLn s
                  Right _ -> putStrLn "All tests passed."

eq :: (Eq a, Show a) => Int -> a -> a -> Either String ()
eq n actual expected = if actual == expected
                           then Right ()
                           else Left errMsg
    where errMsg = unlines [ "Test " ++ show n
                           , "  Expected:    " ++ show expected
                           , "  But got:     " ++ show actual
                           ]
