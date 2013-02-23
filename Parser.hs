module Parser
( parse
) where

import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec (parse)

expression :: GenParser Char st [String]
expression = do
    result <- many term
    eof
    let filtered = [ x | x <- result, x /= " " ]
    return filtered

term :: GenParser Char st String
term = whitespace <|> (many1 digit) <|> operator

operator = string "d"

whitespace = string " "

--

parse :: String -> Either ParseError [String]
parse input = Text.ParserCombinators.Parsec.parse expression "" input
