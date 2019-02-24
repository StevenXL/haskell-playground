{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module MonadicParserCombinators.Lib02 where

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

word :: Parser String
word = neWord `plus` result ""
  where
    neWord = letter `bind` \l -> word `bind` \w -> result (l : w)

-- word :: Parser String
-- word = letter `bind` \l -> word `bind` \w -> result (l:w)
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
