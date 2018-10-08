-- Monad Transformers - Step By Step (https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf)
module MonadTStepByStep.Transformers04 where

import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Reader   (ReaderT, ask, local, runReaderT)
import           Control.Monad.State    (StateT, runStateT, MonadState(..))
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
type AppState = Int

-- ExceptT InterpreterError (StateT AppState Identity) a 
-- runExceptT :: ExceptT InterpreterError (StateT AppState Identity) a -> StateT AppState Identity (Either InterpreterError a)
-- Step 7: Add an Environment
type Eval4 a = ReaderT Env (ExceptT InterpreterError (StateT AppState Identity)) a

runEval4 :: Env -> AppState -> Eval4 a -> (Either InterpreterError a, AppState)
runEval4 env appState eval = runIdentity $ runStateT ( runExceptT $ runReaderT eval env ) appState

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = tick >> (return $ IntVal i)
eval4 (Var name) = do
  tick
  env <- ask
  maybe (throwError $ "Unbound variable: " ++ name) return (M.lookup name env)
eval4 (Plus exp exp') = do
  tick
  v1 <- eval4 exp
  v2 <- eval4 exp'
  case (v1, v2) of
    (IntVal i, IntVal i') -> return (IntVal $ i + i')
    _ -> throwError $ "Type Error: " ++ show v1 ++ " or " ++ show v2 ++ " is not an IntVal"
eval4 (Abs name exp) = do
  tick
  env <- ask
  return $ FunVal env name exp
eval4 (App exp exp') = do
  tick
  fun <- eval4 exp
  val2 <- eval4 exp'
  case fun of
    FunVal env' n body -> local (const $ M.insert n val2 env' ) (eval4 body)
    _ -> throwError $ "Type Error: " ++ show fun ++ " is not a function"

tick :: (Num s, MonadState s m) => m ()
tick = do
  calls <- get
  put (calls + 1)

test4 :: IO ()
test4 = print $ runEval4 M.empty 0 (eval4 exampleExp)
