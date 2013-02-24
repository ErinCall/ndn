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
                 | Number Int
    deriving (Eq, Show)

expression :: GenParser Char st [ParseResult]
expression = do
    result <- many term
    eof
    return result

term :: GenParser Char st ParseResult
term = number <|> operator

number = do
    num <- many1 digit
    return $ Number $ read num

operator = (string "d" >> return DieRoll)
       <|> (string "+" >> return Add)
       <|> (string "*" >> return Multiply)
       <|> (string "-" >> return Subtract)

--

parse :: String -> Either ParseError [ParseResult]
parse input = Text.ParserCombinators.Parsec.parse expression "" input
