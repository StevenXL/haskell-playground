module BasicsOfHaskell.SymbolicCalculator where

import qualified Data.Char as Char

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

tokenizeChar :: Char -> Token
tokenizeChar c
  | c `elem` "+-/*" = TokenOp (operator c)
  | Char.isDigit c = TokenNumber (Char.digitToInt c)
  | Char.isAlpha c = TokenIdentifier [c]
  | Char.isSpace c = TokenSpace
  | otherwise = error (unwords ["Cannot tokenize charachter", [c], "."])

tokenize :: String -> [Token]
tokenize = map tokenizeChar

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
