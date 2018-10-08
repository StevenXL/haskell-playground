-- Monad Transformers - Step By Step (https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf)
module MonadTStepByStep.Transformers02 where

import Data.Maybe (fromJust, maybe)
import           Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Except (ExceptT, throwError, runExceptT)

-- STEP 1: Define the types of our programming language.

type Name = String

type Env = Map Name Value

data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)

data Value = IntVal Integer | FunVal Env Name Exp deriving (Show)

-- STEP 5: Adding error handling.
type InterpreterError = String

type Eval2 a = ExceptT InterpreterError Identity a

runEval2 :: Eval2 a -> Either InterpreterError a
runEval2 eval2 = runIdentity $ runExceptT eval2

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var name) = maybe (throwError $ "Unbound variable: " ++ name) return (M.lookup name env)
eval2 env (Plus exp exp') = do
  v1 <- eval2 env exp
  v2 <- eval2 env exp'
  case (v1, v2) of
    (IntVal i, IntVal i') -> return (IntVal $ i + i')
    _ -> throwError $ "Type Error: " ++ show v1 ++ " or " ++ show v2 ++ " is not an IntVal"
eval2 env (Abs name exp) = return $ FunVal env name exp
eval2 env (App exp exp') = do
  fun <- eval2 env exp
  val2 <- eval2 env exp'
  case fun of
    FunVal env' n body -> eval2 (M.insert n val2 env') body
    _ -> throwError $ "Type Error: " ++ show fun ++ " is not a function"

exampleExp :: Exp
exampleExp = Plus (Lit 1) (Abs "x" (Var "x"))

test2 :: IO ()
test2 = print $ runEval2 (eval2 M.empty exampleExp)
