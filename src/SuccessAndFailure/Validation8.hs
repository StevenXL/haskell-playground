{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module SuccessAndFailure.Validation8 where

-- Sort imports
-- Qualify imports
import qualified Data.Char       as Char
import qualified Data.List       as List
import qualified Data.Validation as Validation

import Data.Validation (Validation(..))

main :: IO ()
main = do
    userName <- prompt "Please enter your username." *> (Username <$> getLine)
    pass <- prompt "Please enter your password." *> (Password <$> getLine)
    display userName pass

display :: Username -> Password -> IO ()
display userName passWord = case makeUser userName passWord of
                                Success user -> putStrLn ("Welcome " <> show user)
                                Failure err  -> putStr (errorCoerce err)

-- VALIDATION FUNCTIONS
makeUser :: Username -> Password -> Validation Error User
makeUser userName passWord = User <$> userErrors userName <*> passwordErrors passWord

validatePassword :: Password -> Validation Error Password
validatePassword (Password pass) = case cleanWhitespace pass of
                                       Failure f -> Failure f
                                       Success s -> requireAlpha s *> checkPasswordLength s

passwordErrors :: Password -> Validation Error Password
passwordErrors pass = case validatePassword pass of
                          Success s -> Success s
                          Failure f -> Failure (makeError "Invalid password:" <> f)

validateUsername :: Username -> Validation Error Username
validateUsername (Username userName) = case cleanWhitespace userName of
                                           Failure f -> Failure f
                                           Success s -> requireAlpha s *> checkUsernameLength s

userErrors :: Username -> Validation Error Username
userErrors s = case validateUsername s of
                   Success s -> Success s
                   Failure f -> Failure (makeError "Invalid username:" <> f)

makeError :: String -> Error
makeError s = Error s

-- TYPES
data User = User Username Password

instance Show User where
    show :: User -> String
    show (User userName passWord) = show userName

newtype Error = Error String deriving (Show, Eq)

instance Semigroup Error where
    (<>) :: Error -> Error -> Error
    (<>) e1 e2 = makeError newStr
        where newStr = errorCoerce e1 <> "\n" <> errorCoerce e2

errorCoerce :: Error -> String
errorCoerce (Error xxs) = xxs

newtype Password = Password String deriving (Show, Eq)

newtype Username = Username String

userNameCoerce :: Username -> String
userNameCoerce (Username userName) = userName

instance Show Username where
    show :: Username -> String
    show userName = userNameCoerce userName


-- VALIDATION RULES

checkPasswordLength :: String -> Validation Error Password
checkPasswordLength s = case (length s > 20 || length s < 10) of
                               True  -> Failure (makeError "Must be between 10 and 20 characters.")
                               False -> Success (Password s)

requireAlpha :: String -> Validation Error String
requireAlpha s = case (List.all Char.isAlphaNum s) of
                     False -> Failure (makeError "Cannot contain whitespace or special characters.")
                     True  -> Success s

cleanWhitespace :: String -> Validation Error String
cleanWhitespace "" = Failure (makeError "Cannot be empty.")
cleanWhitespace s@(x:xs) = case (Char.isSpace x) of
                             True  -> cleanWhitespace xs
                             False -> Success s

checkUsernameLength :: String -> Validation Error Username
checkUsernameLength name = case (length name > 15) of
                               True -> Failure (makeError "Cannot be longer than 15 characters.")
                               False -> Success (Username name)

prompt :: String -> IO ()
prompt s = putStr $ s <> "\n> "

getPass :: IO String
getPass = getLine

reverseLine :: IO ()
reverseLine = do
  line <- getLine
  print (reverse line)

-- TESTING FRAMEWORK
printTest :: Validation String () -> IO ()
printTest e = case e of
                  Failure s  -> putStrLn s
                  Success _ -> putStrLn "All tests passed."

eq :: (Eq a, Show a) => Int -> a -> a -> Validation String ()
eq n actual expected = if actual == expected
                           then Success ()
                           else Failure errMsg
    where errMsg = unlines [ "Test " ++ show n
                           , "  Expected:    " ++ show expected
                           , "  But got:     " ++ show actual
                           ]
