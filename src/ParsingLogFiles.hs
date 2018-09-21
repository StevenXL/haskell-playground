{-# LANGUAGE OverloadedStrings #-}

module ParsingLogFiles where

import Data.Word (Word8)
import Data.Time (LocalTime(..), fromGregorian, TimeOfDay(..))
import Data.Attoparsec.ByteString.Char8 (char, digit, string, endOfLine, Parser, parseOnly, many', count, decimal)
import Control.Applicative ((<|>))
import qualified Data.ByteString as B

data IP = IP Word8 Word8 Word8 Word8 deriving Show

data Product = Mouse | Keyboard | Monitor | Speakers deriving Show

data LogEntry = LogEntry { entryTime :: LocalTime
                         , entryIP :: IP
                         , entryProduct :: Product
                         } deriving Show

type Log = [LogEntry]

main :: IO ()
main = print $ parseOnly logParser logs

logParser :: Parser Log
logParser = many' $ logEntryParser <* endOfLine

logEntryParser :: Parser LogEntry
logEntryParser = do
  entryTime <- timeParser
  char ' '
  entryIP <- ipParser
  char ' '
  entryProduct <- productParser
  return $ LogEntry entryTime entryIP entryProduct

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

logs :: B.ByteString
logs = B.concat [ "2013-06-29 11:16:23 124.67.34.60 keyboard\n"
              , "2013-06-29 11:32:12 212.141.23.67 mouse\n"
              , "2013-06-29 11:33:08 212.141.23.67 monitor\n"
              , "2013-06-29 12:12:34 125.80.32.31 speakers\n"
              , "2013-06-29 12:51:50 101.40.50.62 keyboard\n"
              , "2013-06-29 13:10:45 103.29.60.13 mouse"
              ]


{-
 -}
