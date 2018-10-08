-- Monad Transformers - Step By Step (https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf)
module MonadTStepByStep.Transformers05 where

import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Reader   (ReaderT, ask, local, runReaderT)
import           Control.Monad.State    (MonadState (..), StateT, runStateT)
import           Control.Monad.Writer   (WriterT, runWriterT, tell)
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
type Log = [String]

-- Step 8: Add loggin capabilities
type Eval5 a = ReaderT Env (ExceptT InterpreterError (WriterT Log (StateT AppState Identity))) a

runEval5 :: Env -> AppState -> Eval5 a -> ((Either InterpreterError a, Log), AppState)
runEval5 env appState eval = runIdentity $ runStateT ( runWriterT $ runExceptT $ runReaderT eval env ) appState

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = tick >> (return $ IntVal i)
eval5 (Var name) = do
  tick
  tell [name]
  env <- ask
  maybe (throwError $ "Unbound variable: " ++ name) return (M.lookup name env)
eval5 (Plus exp exp') = do
  tick
  v1 <- eval5 exp
  v2 <- eval5 exp'
  case (v1, v2) of
    (IntVal i, IntVal i') -> return (IntVal $ i + i')
    _ -> throwError $ "Type Error: " ++ show v1 ++ " or " ++ show v2 ++ " is not an IntVal"
eval5 (Abs name exp) = do
  tick
  tell [name]
  env <- ask
  return $ FunVal env name exp
eval5 (App exp exp') = do
  tick
  fun <- eval5 exp
  val2 <- eval5 exp'
  case fun of
    FunVal env' n body -> local (const $ M.insert n val2 env' ) (eval5 body)
    _ -> throwError $ "Type Error: " ++ show fun ++ " is not a function"

tick :: (Num s, MonadState s m) => m ()
tick = do
  calls <- get
  put (calls + 1)

test5 :: IO ()
test5 = print $ runEval5 M.empty 0 (eval5 exampleExp)
