{-# LANGUAGE OverloadedStrings #-}

module ParsingLogFiles where

import Data.Maybe (maybe)
import Data.Either (either)
import Data.Word (Word8)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)
import Data.Time (LocalTime(..), fromGregorian, TimeOfDay(..))
import Data.Attoparsec.ByteString.Char8 (char, digit, string, endOfLine, Parser, parseOnly, many', count, decimal, option)
import Control.Applicative ((<|>))
import qualified Data.ByteString as B

data IP = IP Word8 Word8 Word8 Word8 deriving Show

data Product = Mouse | Keyboard | Monitor | Speakers deriving (Show, Enum, Eq)

productFromID :: Int -> Product
productFromID id = toEnum (id - 1)

data Source = Internet | Friend | NoAnswer deriving Show

data LogEntry = LogEntry { entryTime :: LocalTime
                         , entryIP :: IP
                         , entryProduct :: Product
                         , entrySource :: Source
                         } deriving Show

type Log = [LogEntry]

main :: IO ()
main = either printErrorMsg printMostSold parseLogs
  where printErrorMsg = print . ("A problem occurred parsing the logs: " <>)
        printMostSold logs = maybe (print "Nothing sold yet") (printBestProduct) (mostSold $ sales logs)
        printBestProduct (p, i) = print $ "The most sold product is (the) " <> show p <> ". It has sold " <> show i <> " units."

parseLogs :: Either String Log
parseLogs = do
  usLogs     <- parseOnly logParserUSFormat (logs <> "\n" <> logsWithSource)
  frenchLogs <- parseOnly logParserFrenchFormat logsInFrench
  return $ merge usLogs frenchLogs

merge :: [a] -> [a] -> [a]
merge = (++)

logParserUSFormat :: Parser Log
logParserUSFormat = many' $ logEntryParser <* endOfLine

logParserFrenchFormat :: Parser Log
logParserFrenchFormat = many' $ logEntryParserFrench <* endOfLine

logEntryParser :: Parser LogEntry
logEntryParser = do
  entryTime    <- timeParser
  char ' '
  entryIP      <- ipParser
  char ' '
  entryProduct <- productParser
  entrySource  <- option NoAnswer $ char ' ' >> sourceParser
  return $ LogEntry entryTime entryIP entryProduct entrySource

logEntryParserFrench :: Parser LogEntry
logEntryParserFrench = do
  entryIP <- ipParser
  char ' '
  entryTime <- timeParserEuropean
  char ' '
  entryProduct <- (productFromID . digitToInt) <$> digit
  char ' '
  entrySource <- sourceParser
  return $ LogEntry entryTime entryIP entryProduct entrySource

timeParser :: Parser LocalTime
timeParser = do
  y  <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  d  <- count 2 digit
  char ' '
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  char ':'
  s  <- count 2 digit
  return $ LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
                     , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
                     }

timeParserEuropean :: Parser LocalTime
timeParserEuropean = do
  d   <- count 2 digit
  char '/'
  m   <- count 2 digit
  char '/'
  y   <- count 4 digit
  char ' '
  h   <- count 2 digit
  char ':'
  mm  <- count 2 digit
  char ':'
  s   <- count 2 digit
  return $ LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
                     , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
                     }


ipParser :: Parser IP
ipParser = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

productParser :: Parser Product
productParser = keyboardParser <|> mouseParser <|> monitorParser <|> speakersParser

keyboardParser :: Parser Product
keyboardParser = string "keyboard" >> return Keyboard

mouseParser :: Parser Product
mouseParser = string "mouse" >> return Mouse

monitorParser :: Parser Product
monitorParser = string "monitor" >> return Monitor

speakersParser :: Parser Product
speakersParser = string "speakers" >> return Speakers

sourceParser :: Parser Source
sourceParser = internetParser <|> friendParser <|> noanswerParser

internetParser :: Parser Source
internetParser = string "internet" >> return Internet

friendParser :: Parser Source
friendParser = string "friend" >> return Friend

noanswerParser :: Parser Source
noanswerParser = string "noanswer" >> return NoAnswer

logs :: B.ByteString
logs = B.concat [ "2013-06-29 11:16:23 124.67.34.60 keyboard\n"
                , "2013-06-29 11:32:12 212.141.23.67 mouse\n"
                , "2013-06-29 11:33:08 212.141.23.67 monitor\n"
                , "2013-06-29 12:12:34 125.80.32.31 speakers\n"
                , "2013-06-29 12:51:50 101.40.50.62 keyboard\n"
                , "2013-06-29 13:10:45 103.29.60.13 mouse"
                ]

logsWithSource :: B.ByteString
logsWithSource = B.concat [ "2014-06-29 11:16:23 124.67.34.60 keyboard internet\n"
                          , "2014-06-29 11:32:12 212.141.23.67 mouse internet\n"
                          , "2014-06-29 11:33:08 212.141.23.67 monitor friend\n"
                          , "2014-06-29 12:12:34 125.80.32.31 speakers noanswer\n"
                          , "2014-06-29 12:51:50 101.40.50.62 keyboard internet\n"
                          , "2014-06-29 13:10:45 103.29.60.13 mouse internet"
                          ]

logsInFrench :: B.ByteString
logsInFrench = B.concat [  "154.41.32.99 29/06/2015 15:32:23 4 internet\n"
                         , "76.125.44.33 29/06/2015 16:56:45 3 noanswer\n"
                         , "123.45.67.89 29/06/2015 18:44:29 4 friend\n"
                         , "100.23.32.41 29/06/2015 19:01:09 1 internet\n"
                         , "151.123.45.67 29/06/2015 20:30:13 2 internet"
                         ]

-- EXTRACTING INFORMATION
type Sales = [(Product, Int)]

salesOf :: Product -> Sales -> Int
salesOf product sales = fromMaybe 0 $ lookup product sales

addSale :: Product -> Sales -> Sales
addSale p [] = [(p,1)]
addSale p ((x,n):xs) = if p == x then (x,n+1):xs
                                 else (x,n) : addSale p xs

mostSold :: Sales -> Maybe (Product, Int)
mostSold [] = Nothing
mostSold sales = Just $ maximumBy compareBySales sales
  where compareBySales x y = snd x `compare` snd y

sales :: Log -> Sales
sales log = foldr toSales [] log
  where toSales logEntry sales = addSale (entryProduct logEntry) sales
