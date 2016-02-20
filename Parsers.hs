module Parsers where

import qualified Control.Monad.Except as E
import Text.ParserCombinators.Parsec hiding (spaces)

import LispData

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = String <$> (parseQuote *> rawString <* parseQuote)
    where rawString = many $ noneOf ['"']

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    let atom = first:rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit

parseQuote :: Parser Char
parseQuote = char '"'

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    x <- char '\'' >> parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseNumber
            <|> parseString
            <|> parseQuoted
            <|> parseList'
    where parseList' = char '(' *> (try parseList <|> parseDottedList) <* char ')'

readExpr :: String -> ThrowsError LispVal
readExpr exp = case parse parseExpr "leme" exp of
    Left err -> E.throwError $ Parser err
    Right val -> return val
