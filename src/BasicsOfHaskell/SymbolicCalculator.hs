module BasicsOfHaskell.SymbolicCalculator where

import qualified Data.Char as Char
import qualified Data.List as List

data Operator
  = Plus
  | Minus
  | Mult
  | Div
  deriving (Eq, Show)

data Token
  = TokenOp Operator
  | TokenIdentifier String
  | TokenNumber Int
  | TokenSpace
  deriving (Eq, Show)

data Expression

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:str)
  | c `elem` "+-/*" = TokenOp (operator c) : tokenize str
  | Char.isDigit c = number c str
  | Char.isAlpha c = identifier c str
  | Char.isSpace c = TokenSpace : tokenize str
  | otherwise = error (unwords ["Cannot tokenize charachter", [c], "."])

identifier :: Char -> String -> [Token]
identifier c str = TokenIdentifier (c : alphaNums) : tokenize rest
  where
    (alphaNums, rest) = List.span (Char.isAlphaNum) str

number :: Char -> String -> [Token]
number c str = TokenNumber number : tokenize rest
  where
    number = read $ c : digits
    (digits, rest) = List.span (Char.isDigit) str

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

operator :: Char -> Operator
operator '+' = Plus
operator '-' = Minus
operator '*' = Mult
operator '/' = Div
operator c =
  error (unwords ["Cannot convert character", [c], "into an operator"])
