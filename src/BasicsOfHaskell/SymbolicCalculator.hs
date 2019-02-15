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
  | TokenAssign
  | TokenLParen
  | TokenRParen
  | TokenIdentifier String
  | TokenNumber Int
  | TokenSpace
  | TokenEnd
  deriving (Eq, Show)

data Expression

data Tree
  = SumNode Operator
            Tree
            Tree
  | ProdNode Operator
             Tree
             Tree
  | AssignNode String
               Tree
  | UnaryNode Operator
              Tree
  | NumNode Double
  | VarNode String
  deriving (Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:str)
  | c `elem` "+-/*" = TokenOp (operator c) : tokenize str
  | c == '=' = TokenAssign : tokenize str
  | c == '(' = TokenLParen : tokenize str
  | c == ')' = TokenRParen : tokenize str
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

-- THE CODE IN THIS SECTION IS OUR PARSER
parse :: [Token] -> Expression
parse = undefined

expression :: [Token] -> (Tree, [Token])
expression tokens =
  let (termTree, toks) = term toks
   in case lookAhead toks of
        (TokenOp op)
          | elem op [Plus, Minus] ->
            let (exTree, toks') = expression (accept toks)
             in (SumNode op termTree exTree, toks')
        TokenAssign ->
          case termTree of
            VarNode str ->
              let (exTree, toks') = expression (accept toks)
               in (AssignNode str exTree, toks')
            _ -> error "Only variables can be assigned to"
        _ -> (termTree, toks)

evaluate :: Expression -> Double
evaluate = undefined

operator :: Char -> Operator
operator '+' = Plus
operator '-' = Minus
operator '*' = Mult
operator '/' = Div
operator c =
  error (unwords ["Cannot convert character", [c], "into an operator"])
