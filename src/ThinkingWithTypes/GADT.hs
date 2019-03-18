{-# LANGUAGE GADTs #-}
module ThinkingWithTypes.GADT where

-- GADT allows us to explicitly define the type of a data constructor.
-- The real power of a GADT is that it allows us to control the return type of a
-- data constructor


data Expr a where
  LiteralInt :: Int -> Expr Int
  LiteralBool :: Bool -> Expr Bool

eval :: Expr a -> a
eval (LiteralInt i) = i
eval (LiteralBool b) = b

main :: IO ()
main = do
  print $ eval (LiteralBool False) -- Here, the return type of eval is a Bool
  print $ eval (LiteralInt 5)      -- Here, the return type of eval is an Int
