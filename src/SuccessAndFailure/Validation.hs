module SuccessAndFailure.Validation where

-- Sort imports
-- Qualify imports
import qualified Data.Char as Char
import qualified Data.List as List

main :: IO ()
main = do
    pass <- prompt "Please enter a password." >> getPass
    print (checkPasswordLength pass)

checkPassword :: String -> Maybe String
checkPassword s0 = case (cleanWhitespace s0) of
                      Nothing -> Nothing
                      Just s1 -> case (requireAlpha s1) of
                                     Nothing -> Nothing
                                     Just s2 -> checkPasswordLength s2
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
