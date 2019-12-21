module SuccessAndFailure.Validation5 where

-- Sort imports
-- Qualify imports
import qualified Data.Char as Char
import qualified Data.List as List

main :: IO ()
main = prompt "Please enter a password." >> getPass >>= print . validatePassword . Password

test :: IO ()
test = printTest $ do
           eq 1 (checkPasswordLength "") (Right $ Password "")
           eq 2 (checkPasswordLength "julielovesbooks") (Right $ Password "julielovesbooks")

validatePassword :: Password -> Either Error Password
validatePassword (Password pass) = do
    cleaned    <- cleanWhitespace pass
    withAlphas <- requireAlpha cleaned
    checkPasswordLength withAlphas

validateUsername :: Username -> Either Error Username
validateUsername (Username userName) = cleanWhitespace userName >>= requireAlpha >>= checkUsernameLength

-- TYPES
newtype Error = Error String deriving (Show, Eq)
newtype Password = Password String deriving (Show, Eq)
newtype Username = Username String deriving Show

-- VALIDATION RULES

checkPasswordLength :: String -> Either Error Password
checkPasswordLength s = case (length s > 20 || length s < 10) of
                               True  -> Left (Error "Your password must be between 10 and 20 characters.")
                               False -> Right (Password s)

requireAlpha :: String -> Either Error String
requireAlpha s = case (List.all Char.isAlphaNum s) of
                     False -> Left (Error "Your password cannot contain whitespace or special characters.")
                     True  -> Right s

cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left (Error "Your password cannot be empty.")
cleanWhitespace s@(x:xs) = case (Char.isSpace x) of
                             True  -> cleanWhitespace xs
                             False -> Right s

checkUsernameLength :: String -> Either Error Username
checkUsernameLength name = case (length name > 15) of
                               True -> Left (Error "Username cannot be longer than 15 characters.")
                               False -> Right (Username name)

checkLength :: Int -> String -> Either Error String
checkLength maxLength s = case (length s > maxLength) of
                              False -> Right s
                              True -> Left $ Error msg
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

-- Exercise 11
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just a) f = f a

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


-- Exercise 12
data StringOrValue a = Str String | Val a deriving Show

bindStringOrValue :: StringOrValue a -> (a -> StringOrValue b) -> StringOrValue b
bindStringOrValue (Str s) _ = Str s
bindStringOrValue (Val a) f = f a

