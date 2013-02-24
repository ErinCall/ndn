module Parser
(
    parse,
    Expression (Number, Add, Multiply, Subtract, DieRoll),
) where

import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec (parse)
import Text.ParserCombinators.Parsec.Expr

data Expression = Number Int
                | Add Expression Expression
                | Subtract Expression Expression
                | Multiply Expression Expression
                | DieRoll Expression Expression
    deriving (Show)

statement :: GenParser Char st Expression
statement = do
    result <- expression
    eof
    return result

expression = buildExpressionParser table factor
             <?> "expression"

table = [[op "*" Multiply AssocLeft, op "d" DieRoll AssocLeft],
         [op "+" Add AssocLeft,      op "-" Subtract AssocLeft]]
      where
        op s f assoc = Infix (string s >> return f) assoc

factor = do
    char '('
    x <- expression
    char ')'
    return x
    <|> number
    <?> "simple expression"

number = do
    num <- many1 digit
    return $ Number $ read num
    <?> "number"

--

parse :: String -> Either ParseError Expression
parse input = Text.ParserCombinators.Parsec.parse statement "" input
