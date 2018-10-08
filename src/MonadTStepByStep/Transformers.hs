-- Monad Transformers - Step By Step (https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf)
module MonadTStepByStep.Transformers where

import Data.Maybe (fromJust, maybe)
import           Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Identity (Identity, runIdentity)

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

-- STEP 2: Write an evaluation / interpreter function for our programming
-- language. Naturally, the type has to be Env -> Exp -> Value
eval0 :: Env -> Exp -> Value
eval0 _ (Lit i) = IntVal i
eval0 env (Var name) = fromJust (M.lookup name env)               -- partiality
eval0 env (Plus exp exp') = let IntVal i = eval0 env exp          -- partiality
                                IntVal i' = eval0 env exp'        -- partiality
                            in IntVal (i + i')
eval0 env (Abs name exp) = FunVal env name exp
eval0 env (App exp exp') = let FunVal env' n body = eval0 env exp -- partiality
                               val2 = eval0 env exp'
                           in eval0 (M.insert n val2 env') body

-- STEP 3: Write a test expression
exampleExp :: Exp
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

test0 :: IO ()
test0 = print $ eval0 M.empty exampleExp

-- STEP 4: In order to use monad transformers, we have to express functions in a
-- monadic style. Here we define our eval0 function in monadi style with the
-- Identity monad. Because the Identity monad has no effect, we can consider it
-- the base case.

-- STEP 4A: Define our base case monad.
type Eval1 a = Identity a

-- STEP 4B: The execution function for our monad, just so that we "hide" the
-- fact that it is the Identity monad underneath the hood.
runEval1 :: Eval1 a -> a
runEval1 eval1 = runIdentity eval1

-- STEP 4C: Re-write eval0 in monad style:

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var name) = maybe (fail "Error") return (M.lookup name env)
eval1 env (Plus exp exp') = do
  IntVal i  <- eval1 env exp
  IntVal i' <- eval1 env exp'
  return (IntVal $ i + i')
eval1 env (Abs name exp) = return $ FunVal env name exp
eval1 env (App exp exp') = do
  FunVal env' n body <- eval1 env exp
  val2               <- eval1 env exp'
  eval1 (M.insert n val2 env') body

test1 :: IO ()
test1 = print $ runEval1 (eval1 M.empty exampleExp)
