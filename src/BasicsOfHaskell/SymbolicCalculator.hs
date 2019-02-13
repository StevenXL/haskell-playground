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
  deriving (Eq, Show)

data Expression

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:str)
  | c `elem` "+-/*" = TokenOp (operator c) : tokenize str
  | Char.isDigit c = TokenNumber (Char.digitToInt c) : tokenize str
  | Char.isAlpha c = TokenIdentifier [c] : tokenize str
  | Char.isSpace c = tokenize str
  | otherwise = error (unwords ["Cannot tokenize charachter", [c], "."])

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
