module BasicsOfHaskell.SymbolicCalculator where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map

newtype Evaluator a =
  Evaluator (Either String a)
  deriving (Show)

instance Monad Evaluator where
  (>>=) = bindE

instance Applicative Evaluator where
  pure = returnE
  (<*>) = Monad.ap

instance Functor Evaluator where
  fmap = Monad.liftM

bindE :: Evaluator a -> (a -> Evaluator b) -> Evaluator b
bindE (Evaluator (Left str)) _ = Evaluator $ Left str
bindE (Evaluator (Right a)) k = k a

returnE :: a -> Evaluator a
returnE = Evaluator . Right

failE :: String -> Evaluator a
failE = Evaluator . Left

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
  | TokenEnd
  deriving (Eq, Show)

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

type SymbolTable = Map.Map String Double

-- THE CODE IN THIS SECTION IS OUR LEXER
-- A LEXER CONVERTS OUR STRING INTO TOKENS
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:str)
  | c `elem` "+-/*" = TokenOp (operator c) : tokenize str
  | c == '=' = TokenAssign : tokenize str
  | c == '(' = TokenLParen : tokenize str
  | c == ')' = TokenRParen : tokenize str
  | Char.isDigit c = number c str
  | Char.isAlpha c = identifier c str
  | Char.isSpace c = tokenize str
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
parse :: [Token] -> Tree
parse toks =
  let (tree, toks') = expression toks
   in if null toks'
        then tree
        else error $ "Leftover tokens: " ++ show toks'

expression :: [Token] -> (Tree, [Token])
expression tokens =
  let (termTree, toks) = term tokens
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

term :: [Token] -> (Tree, [Token])
term tokens =
  let (factorTree, toks) = factor tokens
   in case lookAhead toks of
        (TokenOp op)
          | elem op [Mult, Div] ->
            let (termTree, toks') = term (accept toks)
             in (ProdNode op factorTree termTree, toks')
        _ -> (factorTree, toks)

factor :: [Token] -> (Tree, [Token])
factor tokens =
  case lookAhead tokens of
    (TokenNumber x) -> (NumNode $ fromInteger $ toInteger x, accept tokens)
    (TokenIdentifier str) -> (VarNode str, accept tokens)
    (TokenOp op)
      | elem op [Plus, Minus] ->
        let (factTree, toks) = factor (accept tokens)
         in (UnaryNode op factTree, toks)
    TokenLParen ->
      let (expTree, toks') = expression (accept tokens)
       in if lookAhead toks' /= TokenRParen
            then error "Missing right parenthesis"
            else (expTree, accept toks')
    _ -> error $ "Parse error on tokens: " ++ show tokens

lookAhead :: [Token] -> Token
lookAhead [] = TokenEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

evaluate :: Tree -> SymbolTable -> Evaluator (Double, SymbolTable)
evaluate (SumNode op lTree rTree) symTab =
  evaluate lTree symTab `bindE` \(left, symTab') ->
    evaluate rTree symTab' `bindE` \(right, symTab'') ->
      case op of
        Minus -> returnE (left - right, symTab'')
        Plus -> returnE (left + right, symTab'')
evaluate (ProdNode op lTree rTree) symTab =
  evaluate lTree symTab `bindE` \(left, symTab') ->
    evaluate rTree symTab' `bindE` \(right, symTab'') ->
      case op of
        Mult -> returnE (left * right, symTab'')
        Div -> returnE (left / right, symTab'')
evaluate (AssignNode str tree) symTab =
  evaluate tree symTab `bindE` \(v, symTab') ->
    addSymbol str v symTab' `bindE` \(_, symTab'') -> returnE (v, symTab'')
evaluate (UnaryNode op tree) symTab =
  evaluate tree symTab `bindE` \(v, symTab') ->
    case op of
      Plus -> returnE (v, symTab')
      Minus -> returnE (negate v, symTab')
evaluate (NumNode d) symTab = returnE (d, symTab)
evaluate (VarNode str) symTab =
  case Map.lookup str symTab of
    Just v -> returnE (v, symTab)
    Nothing -> failE $ "Undefined variable " ++ str

addSymbol :: String -> Double -> SymbolTable -> Evaluator ((), SymbolTable)
addSymbol name val symTab = returnE $ ((), Map.insert name val symTab)

operator :: Char -> Operator
operator '+' = Plus
operator '-' = Minus
operator '*' = Mult
operator '/' = Div
operator c =
  error (unwords ["Cannot convert character", [c], "into an operator"])
