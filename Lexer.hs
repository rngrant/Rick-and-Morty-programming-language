module Lexer where

import Control.Monad 
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*))
import CodeGen

paren = do
  whitespace
  void $ string "you gotta"
  whitespace
  e <- pexp
  whitespace
  void $ string "Morty"
  return (EParens e)

whitespace = void $ many $ oneOf " \n\t"

pmul = do
  whitespace
  e1 <- pterm
  whitespace
  string "times"
  whitespace
  e2 <- pexp'
  whitespace
  return $ EBin Mul e1 e2

pdiv = do
  whitespace
  e1 <- pterm
  whitespace
  string "divided by"
  whitespace
  e2 <- pexp'
  whitespace
  return $ EBin Div e1 e2

padd = do
  whitespace
  e1 <- pexp'
  whitespace
  string "plus"
  whitespace
  e2 <- pexp
  whitespace
  return $ EBin Add e1 e2

psub = do
  whitespace
  e1 <- pexp'
  whitespace
  string "minus"
  whitespace
  e2 <- pexp
  whitespace
  return $ EBin Sub e1 e2

pexp = try padd <|> try psub <|> pexp'

pexp' = try pmul <|> try pdiv <|> pterm

--pexp  -> pexp'  + pexp | pexp'  - pexp | pexp'
--pexp' -> pterm  * pexp' | pterm `div` pexp' | pterm
--pterm -> base | (pexp)

pterm = try base <|> paren  --term
  where base = pint

pint :: Parser Exp
pint = do
  n <- many1 digit
  return $ EIntLit (read n)

parseExp :: String -> Either ParseError Exp
parseExp src = parse (pexp <* eof) "" src
