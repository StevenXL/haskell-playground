module Calculator where

data Exp
  = Lit Int
  | Add Exp
        Exp
  | Sub Exp
        Exp
  | Mult Exp
         Exp

evaluate :: Exp -> Int
evaluate (Lit int) = int
evaluate (Add expA expB) = evaluate expA + evaluate expB
evaluate (Sub expA expB) = evaluate expA - evaluate expB
evaluate (Mult expA expB) = evaluate expA * evaluate expB

testExp1 :: Exp
testExp1 = Add (Lit 1) (Sub (Lit 1) (Lit 2))

showBinary :: String -> Exp -> Exp -> String
showBinary str expA expB =
  unwords [str ++ " (", show expA, ") (", show expB, ")"]

instance Show Exp where
  show (Lit int) = unwords ["Lit", show int]
  show (Add expA expB) = showBinary "Add" expA expB
  show (Sub expA expB) = showBinary "Sub" expA expB
  show (Mult expA expB) = showBinary "Mult" expA expB

main :: IO ()
main = do
  putStrLn (unwords ["Evaluating:", show testExp1])
  print (evaluate testExp1)
