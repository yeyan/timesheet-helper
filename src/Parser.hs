module Parser (parseTimeSheet) where

import Text.Parsec hiding (space, spaces)
import Control.Monad.IO.Class
import Control.Applicative (liftA)

import Data.Char (isSpace)
import Data.Time

type Parser = ParsecT String () IO

space :: Parser Char
space = do
    oneOf " \t"

spaces :: Parser [Char]
spaces = do
    many space

basicDate :: Parser Day
basicDate = do
    y <- count 4 digit
    char '-'
    m <- number
    char '-'
    d <- number
    return $ fromGregorian (read y) m d
    where
        number = do
            value <- try (count 2 digit) <|> count 1 digit
            return $ read value

basicTime :: Parser TimeOfDay
basicTime = do
    h <- number
    char ':'
    m <- number
    s <- option 0 $ char ':' >> number
    return $ TimeOfDay h m $ fromIntegral s
    where
        number :: Parser Int
        number = do
            value <- try (count 2 digit) <|> count 1 digit   
            return $ read value

dateTime = do
    d <- basicDate
    space <|> char 'T'
    t <- basicTime
    return $ LocalTime d t

time :: Parser LocalTime
time = do
    d <- liftIO $ do
        zone <- getCurrentTimeZone
        curr <- getCurrentTime
        return $ localDay $ utcToLocalTime zone curr
    t <- basicTime
    return $ LocalTime d t

entryTime =
    try dateTime <|> try time <?>
        "Expecting timestamp in the format of \"yyyy-M[M]-d[d] h[h]:m[m]:s[s]\" \"h[h]:m[m]:s[s]\" or \"h[h]:m[m]\""

entryName = do
    input <- many $ noneOf "\n\r#"
    return $ strip input
    where
        stripL = dropWhile isSpace
        stripR = reverse . stripL . reverse
        strip  = stripR . stripL

comment = do
    char '#'
    many $ noneOf "\n\r"

entry = do
    t <- entryTime
    n <- option "" $ spaces >> entryName
    optional comment
    endOfLine
    return [(t, n)]

empty :: Parser [(LocalTime, String)]
empty = do
    try comment <|> spaces
    endOfLine
    return []

line = do
    try entry <|> try empty <?> "Expecting entry or comment or empty line"

parseTimeSheet fname = do
    content <- readFile fname
    liftA (liftA concat) $ runParserT (many line) () fname content
