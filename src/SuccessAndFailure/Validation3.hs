module SuccessAndFailure.Validation3 where

-- Sort imports
-- Qualify imports
import qualified Data.Char as Char
import qualified Data.List as List

main :: IO ()
main = do
    pass <- prompt "Please enter a password." >> getPass
    print (validatePassword pass)

test :: IO ()
test = printTest $ do
           eq 1 (checkPasswordLength "") (Right "")
           eq 2 (checkPasswordLength "julielovesbooks") (Right "julielovesbooks")

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

