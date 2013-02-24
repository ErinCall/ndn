module Parser
(
    parse,
    ParseResult (Number, DieRoll, Add, Subtract, Multiply),
) where

import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec (parse)

data ParseResult = DieRoll
                 | Add
                 | Subtract
                 | Multiply
                 | Whitespace
                 | Number String
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

operator = (string "d" >> return DieRoll)
       <|> (string "+" >> return Add)
       <|> (string "*" >> return Multiply)
       <|> (string "-" >> return Subtract)

whitespace = string " " >> return Whitespace

--

parse :: String -> Either ParseError [ParseResult]
parse input = Text.ParserCombinators.Parsec.parse expression "" input
