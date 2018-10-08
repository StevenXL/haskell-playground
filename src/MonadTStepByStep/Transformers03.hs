-- Monad Transformers - Step By Step (https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf)
module MonadTStepByStep.Transformers03 where

import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Reader   (ReaderT, runReaderT, ask, local)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust, maybe)

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

exampleExp :: Exp
exampleExp = Plus (Lit 1) (Abs "x" (Var "x"))

type InterpreterError = String

-- Step 6: Adding an environment
type Eval3 a = ReaderT Env (ExceptT InterpreterError Identity) a

runEval3 :: Env -> Eval3 a -> Either InterpreterError a
runEval3 env eval = runIdentity $ runExceptT $ runReaderT eval env

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var name) = do
  env <- ask
  maybe (throwError $ "Unbound variable: " ++ name) return (M.lookup name env)
eval3 (Plus exp exp') = do
  v1 <- eval3 exp
  v2 <- eval3 exp'
  case (v1, v2) of
    (IntVal i, IntVal i') -> return (IntVal $ i + i')
    _ -> throwError $ "Type Error: " ++ show v1 ++ " or " ++ show v2 ++ " is not an IntVal"
eval3 (Abs name exp) = do
  env <- ask
  return $ FunVal env name exp
eval3 (App exp exp') = do
  fun <- eval3 exp
  val2 <- eval3 exp'
  case fun of
    FunVal env' n body -> local (const $ M.insert n val2 env' ) (eval3 body)
    _ -> throwError $ "Type Error: " ++ show fun ++ " is not a function"

test3 :: IO ()
test3 = print $ runEval3 M.empty (eval3 exampleExp)
