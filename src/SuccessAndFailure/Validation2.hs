module SuccessAndFailure.Validation2 where

-- Sort imports
-- Qualify imports
import qualified Data.Char as Char
import qualified Data.List as List

main :: IO ()
main = do
    pass <- prompt "Please enter a password." >> getPass
    print (validatePassword pass)

validatePassword :: String -> Either String String
validatePassword pass = cleanWhitespace pass >>= requireAlpha >>= checkPasswordLength

-- PASSWORD VALIDATION RULES

checkPasswordLength :: String -> Either String String
checkPasswordLength s = case (length s > 20 || length s < 10) of
                               True  -> Left "Your password must be between 10 and 20 characters."
                               False -> Right s

requireAlpha :: String -> Either String String
requireAlpha s = case (List.all Char.isAlphaNum s) of
                     False -> Left "Your password cannot contain whitespace or special characters."
                     True  -> Right s

cleanWhitespace :: String -> Either String String
cleanWhitespace "" = Left "Your password cannot be empty."
cleanWhitespace s@(x:xs) = case (Char.isSpace x) of
                             True  -> cleanWhitespace xs
                             False -> Right s

prompt :: String -> IO ()
prompt s = putStr $ s <> "\n> "

getPass :: IO String
getPass = getLine

reverseLine :: IO ()
reverseLine = do
  line <- getLine
  print (reverse line)

-- Exercise 11
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just a) f = f a

-- Exercise 12
data StringOrValue a = Str String | Val a deriving Show

bindStringOrValue :: StringOrValue a -> (a -> StringOrValue b) -> StringOrValue b
bindStringOrValue (Str s) _ = Str s
bindStringOrValue (Val a) f = f a
