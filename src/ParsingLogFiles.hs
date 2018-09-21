{-# LANGUAGE OverloadedStrings #-}

module ParsingLogFiles where

import Data.Word (Word8)
import Data.Time (LocalTime(..), fromGregorian, TimeOfDay(..))
import Data.Attoparsec.ByteString.Char8 (char, digit, string, endOfLine, Parser, parseOnly, many', count, decimal, option)
import Control.Applicative ((<|>))
import qualified Data.ByteString as B

data IP = IP Word8 Word8 Word8 Word8 deriving Show

data Product = Mouse | Keyboard | Monitor | Speakers deriving Show

data Source = Internet | Friend | NoAnswer deriving Show

data LogEntry = LogEntry { entryTime :: LocalTime
                         , entryIP :: IP
                         , entryProduct :: Product
                         , entrySource :: Source
                         } deriving Show

type Log = [LogEntry]

main :: IO ()
main = print $ parseOnly logParser (logs <> "\n" <> logsWithSource)

logParser :: Parser Log
logParser = many' $ logEntryParser <* endOfLine

logEntryParser :: Parser LogEntry
logEntryParser = do
  entryTime <- timeParser
  char ' '
  entryIP <- ipParser
  char ' '
  entryProduct <- productParser
  entrySource <- option NoAnswer $ char ' ' >> sourceParser
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
logsWithSource = B.concat [ "2013-06-29 11:16:23 124.67.34.60 keyboard internet\n"
                          , "2013-06-29 11:32:12 212.141.23.67 mouse internet\n"
                          , "2013-06-29 11:33:08 212.141.23.67 monitor friend\n"
                          , "2013-06-29 12:12:34 125.80.32.31 speakers noanswer\n"
                          , "2013-06-29 12:51:50 101.40.50.62 keyboard internet\n"
                          , "2013-06-29 13:10:45 103.29.60.13 mouse internet"
                          ]
