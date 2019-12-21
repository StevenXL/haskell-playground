{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SuccessAndFailure.Validation7 where

-- Sort imports
-- Qualify imports
import qualified Data.Char       as Char
import qualified Data.List       as List
import qualified Data.Validation as Validation

import Data.Validation (Validation(..))

main :: IO ()
main = do
    userName <- prompt "Please enter your username." >> (Username <$> getLine)
    pass <- prompt "Please enter your password." >> (Password <$> getLine)
    print (validateUsername userName)
    print (validatePassword pass)

-- VALIDATION FUNCTIONS
makeUser :: Username -> Password -> Validation Error User
makeUser userName passWord = User <$> validateUsername userName <*> validatePassword passWord

validatePassword :: Password -> Validation Error Password
validatePassword (Password pass) = case cleanWhitespace pass of
                                       Failure f -> Failure f
                                       Success s -> requireAlpha s *> checkPasswordLength s

validateUsername :: Username -> Validation Error Username
validateUsername (Username userName) = case cleanWhitespace userName of
                                           Failure f -> Failure f
                                           Success s -> requireAlpha s *> checkUsernameLength s

makeError :: String -> Error
makeError s = Error [s]

-- TYPES
data User = User Username Password
newtype Error = Error [String] deriving (Show, Eq, Semigroup)
newtype Password = Password String deriving (Show, Eq)
newtype Username = Username String deriving Show


-- VALIDATION RULES

checkPasswordLength :: String -> Validation Error Password
checkPasswordLength s = case (length s > 20 || length s < 10) of
                               True  -> Failure (makeError "Your password must be between 10 and 20 characters.")
                               False -> Success (Password s)

requireAlpha :: String -> Validation Error String
requireAlpha s = case (List.all Char.isAlphaNum s) of
                     False -> Failure (makeError "Your password cannot contain whitespace or special characters.")
                     True  -> Success s

cleanWhitespace :: String -> Validation Error String
cleanWhitespace "" = Failure (makeError "Your password cannot be empty.")
cleanWhitespace s@(x:xs) = case (Char.isSpace x) of
                             True  -> cleanWhitespace xs
                             False -> Success s

checkUsernameLength :: String -> Validation Error Username
checkUsernameLength name = case (length name > 15) of
                               True -> Failure (makeError "Username cannot be longer than 15 characters.")
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
