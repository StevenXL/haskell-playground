{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module MonadicParserCombinators.Lib03 where

-- TYPE OF PARSER
newtype Parser a = Parser
  { runParser :: String -> [(a, String)]
  }

-- THREE SIMPLE PARSERS
result :: a -> Parser a
result a = Parser f
  where
    f = \inp -> [(a, inp)]

zero :: Parser a
zero = Parser f
  where
    f = \inp -> []

item :: Parser Char
item = Parser f
  where
    f =
      \inp ->
        case inp of
          [] -> []
          (x:out) -> [(x, out)]

-- TWO BASIC PARSER COMBINATORS
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p k = Parser f
  where
    f = \inp -> concat [runParser (k a) out | (a, out) <- runParser p inp]

-- plus is going to end up giving us ALL the results of both parsers; if both
-- parsers succeed, then we get multiple results
plus :: Parser a -> Parser a -> Parser a
plus p q = Parser f
  where
    f = \inp -> (runParser p inp ++ runParser q inp)

-- OTHER PARSERS BUILT FROM COMBINATION OF SIMPLE PARSERS AND BASIC PARSER
-- COMBINATOR
sat :: (Char -> Bool) -> Parser Char
sat p = item `bind` k
  where
    k =
      \c ->
        if p c
          then result c
          else zero

char :: Char -> Parser Char
char c = sat (== c)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = lower `plus` upper

alphaNum :: Parser Char
alphaNum = letter `plus` digit

-- ALL OF THESE ARE DEFINED IN DATA.CHAR; WE ARE DOING THIS FOR FUN
-- isBetween is a closed interval
isBetween :: Char -> Char -> Char -> Bool
isBetween left right ind = left <= ind && ind <= right

isDigit :: Char -> Bool
isDigit = isBetween '0' '9'

isLower :: Char -> Bool
isLower = isBetween 'a' 'z'

isUpper :: Char -> Bool
isUpper = isBetween 'A' 'Z'

-- PARSER IS A MONAD
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = p >>= \a -> return (f a)

instance Applicative Parser where
  pure :: a -> Parser a
  pure = return
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p <*> q = p >>= \f -> fmap f q

instance Monad Parser where
  return :: a -> Parser a
  return = result
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) = bind

-- PARSER COMBINATORS THAT ARE EASIER TO DEFINE THANKS TO MONAD COMPREHENSION /
-- DO NOTATION
string :: String -> Parser String
string "" = return ""
string (c:cs) = do
  _ <- char c
  _ <- string cs
  return (c : cs)

string' :: String -> Parser String
string' [] = return []
string' (c:cs) = char c >>= \c' -> string' cs >>= \cs' -> return (c' : cs')

-- COMBINATORS FOR REPETITION
many :: Parser a -> Parser [a]
many p = neP `plus` return []
  where
    neP = do
      a <- p
      as <- many p
      return (a : as)

word :: Parser String
word = many letter

digits :: Parser String
digits = many digit

many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p
  return (a : as)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  a <- p
  as <- many (sep >> p)
  return (a : as)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (sepBy1 p sep) `plus` return []

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket left p right = do
  _ <- left
  b <- p
  _ <- right
  return b

-- PARSERS BASED ON REPETITION
nat :: Parser Int
nat = fmap read $ many1 digit

int :: Parser Int
int = do
  op <- char '-' `plus` return '+'
  i <- nat
  case op of
    '-' -> return (negate i)
    '+' -> return i

listOfInts :: Parser [Int]
listOfInts = bracket openingBracket ints closingBracket
  where
    openingBracket = char '['
    ints = sepBy1 int (char ',')
    closingBracket = char ']'

-- BNF NOTATION
-- expr ::= expr addop factor | factor
-- addop ::= + | -
-- factor ::= nat | ( expr )
-- TRANSLATION OF BNF NOTATION INTO A PARSER
expr :: Parser Int
expr = exprParser `plus` factor
  where
    exprParser = do
      x <- expr -- expr parser is left-recursive; this will never make any progress; see article
      f <- addop
      y <- factor
      return (f x y)

addop :: Parser (Int -> Int -> Int)
addop = do
  op <- plusOrMinus
  case op of
    '+' -> return (+)
    '-' -> return (-)
  where
    plusOrMinus = char '+' `plus` char '-'

factor :: Parser Int
factor = nat `plus` bracketedExpr
  where
    bracketedExpr = bracket openingBracket expr closingBracket
    openingBracket = char '('
    closingBracket = char ')'
