
module Plot.Parse(exprParser) where

import Plot.Structure
import Text.Parsec
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec.Number hiding (binary)

-- Parsec makes parsing expression grammars a cinch. We simply define
-- all of the function names and operator names, and Parsec does all
-- the magic for us.

-- This is just a convenience function to construct a binary operator
-- from a name.
binary :: Monad m => String -> (a -> a -> a) -> Assoc -> Operator String u m a
binary s f a = Infix (f <$ string s) a

-- This parser parses only the names of various functions, which will
-- be treated as a prefix "operator" in our grammar.
functionName :: Monad m => ParsecT String u m Function
functionName = (Sqrt <$ try (string "sqrt") <|>
                Sin  <$ try (string "sin" ) <|>
                Cos  <$ try (string "cos" ) <|>
                Tan  <$ try (string "tan" ) <|>
                Ln   <$ try (string "ln"  )) <?> "function name"

-- A "term" in our expression grammar will be a parenthesized
-- expression, a constant numerical value (again, Parsec makes this
-- trivial), or the variable "x".
term :: Fractional a => Parsec String u (Expr a)
term = (between (char '(') (char ')') expr <?> "parenthesized expression") <|>
       ((Fix . Const) <$> fractional2 False) <|>
       ((Fix Var <$ oneOf "xX") <?> "variable name")

-- This is the table of operators, which includes both the infix
-- operators and the prefix "operators" which are actually just
-- function names.
table :: Monad m => OperatorTable String u m (Expr a)
table = [ [Prefix (func <$> functionName)],
          [binary "*" (.*.) AssocLeft],
          [binary "^" (.^) AssocRight],
          [binary "/" (./) AssocLeft],
          [binary "+" (.+.) AssocLeft, binary "-" (.-) AssocLeft] ]

-- The definition of an "expression" is built up from our definition
-- of a term and from our operator table.
expr :: Fractional a => Parsec String u (Expr a)
expr = buildExpressionParser table term

-- An expression, strictly speaking, should end at the end of the
-- string. Otherwise, strings like "x + 1 the rest of this string is
-- garbage" would parse, since there is a valid and syntactically
-- correct prefix.
exprParser :: Fractional a => Parsec String u (Expr a)
exprParser = expr <* eof
