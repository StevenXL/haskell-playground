module Calculator where

import Control.Monad.Except
import Data.Functor.Identity

data Exp
  = Lit Int
  | Add Exp
        Exp
  | Sub Exp
        Exp
  | Mult Exp
         Exp
  | Div Exp
        Exp

data EvalError =
  DivisionByZero
  deriving (Show)

type Eval = Except EvalError

evaluate :: Exp -> Eval Int
evaluate (Lit int) = return int
evaluate (Add expA expB) = do
  a <- evaluate expA
  b <- evaluate expB
  return $ a + b
evaluate (Sub expA expB) = do
  a <- evaluate expA
  b <- evaluate expB
  return $ a - b
evaluate (Mult expA expB) = do
  a <- evaluate expA
  b <- evaluate expB
  return $ a * b
evaluate (Div expA expB) = do
  a <- evaluate expA
  b <- evaluate expB
  if b == 0
    then throwError DivisionByZero
    else return (a `div` b)

runEval :: Eval Int -> Either EvalError Int
runEval = runIdentity . runExceptT

showBinary :: String -> Exp -> Exp -> String
showBinary str expA expB =
  unwords [str ++ " (", show expA, ") (", show expB, ")"]

instance Show Exp where
  show (Lit int) = unwords ["Lit", show int]
  show (Add expA expB) = showBinary "Add" expA expB
  show (Sub expA expB) = showBinary "Sub" expA expB
  show (Mult expA expB) = showBinary "Mult" expA expB
  show (Div expA expB) = showBinary "Div" expA expB

testExp1 :: Exp
testExp1 = Add (Lit 1) (Sub (Lit 1) (Lit 2))

testExp2 :: Exp
testExp2 = Div (Lit 1) (Lit 2)

testExp3 :: Exp
testExp3 = Div (Lit 1) (Lit 0)

main :: IO ()
main = do
  putStrLn (unwords ["Evaluating:", show testExp1])
  print (runEval $ evaluate testExp1)
  putStrLn (unwords ["Evaluating:", show testExp2])
  print (runEval $ evaluate testExp2)
  putStrLn (unwords ["Evaluating:", show testExp3])
  print (runEval $ evaluate testExp3)
