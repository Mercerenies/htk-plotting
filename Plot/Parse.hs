
module Plot.Parse(exprParser) where

import Plot.Structure
import Text.Parsec
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec.Number hiding (binary)

binary :: Monad m => String -> (a -> a -> a) -> Assoc -> Operator String u m a
binary s f a = Infix (f <$ string s) a

functionName :: Monad m => ParsecT String u m Function
functionName = (Sqrt <$ try (string "sqrt") <|>
                Sin  <$ try (string "sin" ) <|>
                Cos  <$ try (string "cos" ) <|>
                Tan  <$ try (string "tan" ) <|>
                Ln   <$ try (string "ln"  )) <?> "function name"

term :: Fractional a => Parsec String u (Expr a)
term = (between (char '(') (char ')') expr <?> "parenthesized expression") <|>
       ((Fix . Const) <$> fractional2 False) <|>
       ((Fix Var <$ oneOf "xX") <?> "variable name")

table :: Monad m => OperatorTable String u m (Expr a)
table = [ [Prefix (func <$> functionName)],
          [binary "*" (.*.) AssocLeft],
          [binary "^" (.^) AssocRight],
          [binary "/" (./) AssocLeft],
          [binary "+" (.+.) AssocLeft, binary "-" (.-) AssocLeft] ]

expr :: Fractional a => Parsec String u (Expr a)
expr = buildExpressionParser table term

exprParser :: Fractional a => Parsec String u (Expr a)
exprParser = expr <* eof
