module Parser (parseTimeSheet) where

import Text.Parsec
import Text.Parsec.String

time :: Parser Double
time = do
    h <- many1 digit
    char ':'
    m <- many1 digit
    return $ read h + (read m) / 60

line :: Parser (String, Double)
line = do
    many space
    t <- time <?> "time"
    many $ oneOf " "
    l <- option "" $ many1 $ noneOf "\n\r"
    endOfLine
    return (l, t)

parseTimeSheet fileName = do
    parseFromFile (many line) fileName
