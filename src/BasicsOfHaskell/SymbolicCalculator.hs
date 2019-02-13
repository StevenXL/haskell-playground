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
  | Char.isDigit c = TokenNumber (Char.digitToInt c) : tokenize str
  | Char.isAlpha c =
    let (token, string) = identifier c str
     in token : tokenize string
  | Char.isSpace c = TokenSpace : tokenize str
  | otherwise = error (unwords ["Cannot tokenize charachter", [c], "."])

identifier :: Char -> String -> (Token, String)
identifier c str = (identifierToken, string)
  where
    identifierToken = TokenIdentifier (c : restOfToken)
    (restOfToken, string) = List.span (/= ' ') str

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
