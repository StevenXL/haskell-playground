-- Monad Transformers - Step By Step (https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf)
module MonadTStepByStep.Transformers where

import Data.Maybe (fromJust)
import           Data.Map (Map)
import qualified Data.Map as M

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
