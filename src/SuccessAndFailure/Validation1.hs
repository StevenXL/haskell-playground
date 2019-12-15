module SuccessAndFailure.Validation1 where

-- Sort imports
-- Qualify imports
import qualified Data.Char as Char
import qualified Data.List as List

main :: IO ()
main = do
    pass <- prompt "Please enter a password." >> getPass
    print (validatePassword pass)

-- validatePassword :: String -> Maybe String
-- validatePassword s0 = case (cleanWhitespace s0) of
--                       Nothing -> Nothing
--                       Just s1 -> case (requireAlpha s1) of
--                                      Nothing -> Nothing
--                                      Just s2 -> validatePasswordLength s2

validatePassword :: String -> Maybe String
validatePassword s = cleanWhitespace s >>= requireAlpha >>= checkPasswordLength

-- PASSWORD VALIDATION RULES

checkPasswordLength :: String -> Maybe String
checkPasswordLength s = case (length s > 20 || length s < 10) of
                               True  -> Nothing
                               False -> Just s

requireAlpha :: String -> Maybe String
requireAlpha s = case (List.all Char.isAlphaNum s) of
                     False -> Nothing
                     True  -> Just s

cleanWhitespace :: String -> Maybe String
cleanWhitespace "" = Nothing
cleanWhitespace s@(x:xs) = case (Char.isSpace x) of
                             True  -> cleanWhitespace xs
                             False -> Just s

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
