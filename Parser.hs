module Parser
(
    parse,
    ParseResult (DieRoll,Number),
) where

import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec (parse)

data ParseResult = DieRoll | Whitespace | Number String
    deriving (Eq, Show)

expression :: GenParser Char st [ParseResult]
expression = do
    result <- many term
    eof
    let filtered = [ x | x <- result, x /= Whitespace ]
    return filtered

term :: GenParser Char st ParseResult
term = whitespace <|> number <|> operator

number = do
    num <- many1 digit
    return $ Number num

operator = do
    string "d"
    return DieRoll

whitespace = do
    string " "
    return Whitespace

--

parse :: String -> Either ParseError [ParseResult]
parse input = Text.ParserCombinators.Parsec.parse expression "" input
