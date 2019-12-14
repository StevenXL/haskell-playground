module SuccessAndFailure.Exercise where

import qualified Data.Char as Char
import qualified Data.List as List

-- Exercise 1
absVal :: (Num a, Ord a) => a -> a
absVal i = case (i < 0) of
               True -> negate i
               False -> i

-- Exercise 2
validateUsernameAndPassword :: String -> String -> String
validateUsernameAndPassword username password =
  case (null username, null password) of
      (True, True)   -> "Emtpy username and password"
      (True, False)  -> "Empty username"
      (False, True)  -> "Empty password"
      (False, False) -> "Okay"

-- Exercise 4
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

isAnagram :: String -> String -> Bool
isAnagram xs ys = List.sort xs == List.sort ys

isWord :: String -> Maybe String
isWord word
    | null word                  = Nothing
    | List.all Char.isAlpha word = Just word
    | otherwise                  = Nothing

-- This function is not expressive; the intent gets lost in the pattern
-- matching. (The intent is to check if both inputs are words, and only if that
-- is true, check if they are anagrams.
checkAnagram :: String -> String -> String
checkAnagram xs ys =
    case (isWord xs) of
        Nothing -> "The first word is invalid."
        Just x' -> case (isWord ys) of
                       Nothing -> "The second word is invalid."
                       Just y' -> case (isAnagram x' y') of
                                      False -> "These words are not anagrams"
                                      True  -> "These words are anagrams"

checkPalindrome :: String -> String
checkPalindrome s = case (sanitize s) of
                        "" -> "The (sanitized) input is empty."
                        xs -> case (isPalindrome xs) of
                                  False -> "The input is not a palindrome."
                                  True  -> "The input is a palindrome."

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

sanitize :: String -> String
sanitize = map Char.toLower . List.filter Char.isAlpha

main :: IO ()
main = do
    xs <- prompt "Input First Word" >> getWord
    ys <- prompt "Input Second Word" >> getWord
    putStrLn (checkAnagram xs ys)

prompt :: String -> IO ()
prompt s = putStr $ s <> "\n> "

getWord :: IO String
getWord = getLine

palindromeIO :: IO ()
palindromeIO = do
    word <- prompt "Input a word" >> getWord
    putStrLn (checkPalindrome word)

leetSpeakIO :: IO ()
leetSpeakIO = do
    word <- prompt "Input a word" >> getWord
    putStrLn (stringToLeetSpeak word)

stringToLeetSpeak :: String -> String
stringToLeetSpeak = map charToLeetSpeak

charToLeetSpeak :: Char -> Char
charToLeetSpeak c = case Char.toLower c of
                        'e' -> '3'
                        't' -> '7'
                        'l' -> '1'
                        _ -> c
