module Lexer where

import Control.Monad 
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*))
import CodeGen

parenA = do
  whitespace
  void $ string "you gotta"
  whitespace
  e <- opa
  whitespace
  void $ string "Morty"
  whitespace
  return (EParens e)

parenB = do
  whitespace
  void $ string "you gotta"
  whitespace
  e <- opb
  whitespace
  void $ string "Morty"
  whitespace
  return (EParens e)

whitespace = void $ many $ oneOf " \t\n"
crlf = many $ oneOf "\n"

pmul = do
  whitespace
  e1 <- pterm
  whitespace
  string "times"
  whitespace
  e2 <- opa'
  whitespace
  return $ EBin Mul e1 e2

pdiv = do
  whitespace
  e1 <- pterm
  whitespace
  string "divided by"
  whitespace
  e2 <- opa'
  whitespace
  return $ EBin Div e1 e2

pmod = do
  whitespace
  e1 <- pterm
  whitespace
  string "mod"
  whitespace
  e2 <- opa'
  whitespace
  return $ EBin Mod e1 e2

padd = do
  whitespace
  e1 <- opa'
  whitespace
  string "plus"
  whitespace
  e2 <- opa
  whitespace
  return $ EBin Add e1 e2

psub = do
  whitespace
  e1 <- opa'
  whitespace
  string "minus"
  whitespace
  e2 <- opa
  whitespace
  return $ EBin Sub e1 e2

--BOOLEAN OPERATOR PARSER--

band = do
  whitespace
  e1 <- opb'
  whitespace
  string "and"
  whitespace
  e2 <- opb
  whitespace
  return $ EBin And e1 e2

bor = do
  whitespace
  e1 <- opb'
  whitespace
  string "or"
  whitespace
  e2 <- opb
  whitespace
  return $ EBin Or e1 e2

numEq = do
  whitespace
  e1 <- opa
  whitespace
  string "is the same as"
  e2 <- opa
  whitespace
  return $ EBin Equals e1 e2

numLt = do
  whitespace
  e1 <- opa
  whitespace
  string "is less than"
  e2 <- opa
  whitespace
  return $ EBin LessThan e1 e2

numGt = do
  whitespace
  e1 <- opa
  whitespace
  string "is greater than"
  e2 <- opa
  whitespace
  return $ EBin LessThan e1 e2

--STATEMENTS--

sIf = do
  whitespace
  string "if"
  whitespace
  e1 <- opb
  whitespace
  string "then"
  whitespace
  s1 <- stmt `endBy` crlf
  whitespace
  string "otherwise"
  whitespace
  s2 <- stmt `endBy` crlf
  whitespace
  string "wubalubadubdub"
  whitespace
  return $ SIf e1 s1 s2

sDec = do
  whitespace
  s <- many1 letter
  whitespace
  string "means"
  whitespace
  e <- expr
  whitespace
  return $ SDecl s e

sPrint = do
  whitespace
  string "show me"
  whitespace
  s <- many1 letter
  whitespace
  return $ SPrint s

sWhile = do
  whitespace
  string "while"
  whitespace
  e <- opb
  whitespace
  string "do this for grandpa"
  whitespace
  s <- stmt `endBy` crlf
  whitespace
  string "thanks Summer"
  whitespace
  return $ SWhile e s

--GRAMMAR--

--stmt -> if | while | dec
--if -> 'if' opb 'then' [stmt] 'otherwise' [stmt]

--expr -> opa | opb

--opa  -> opa' + opa | opa'  - opa | opa'
--opa' -> pterm  * opa' | pterm `div` opa' | pterm
--pterm -> base | (opa)

--opb  -> opb' = opb | opb' and opb | opb' or opb | opb'
--opb' -> opa = opa | opa < opa | opa > opa | bterm
--bterm -> True | False | (opb)

stmt = try sIf <|> try sDec <|> try sPrint <|> try sWhile

expr = try opb <|> try opa

--NUMBER OPS--

opa = try padd <|> try psub <|> opa'

opa' = try pmul <|> try pdiv <|> try pmod <|> pterm

pterm = try base <|> try parenA <|> try pvar
  where base = pint

pint :: Parser Exp
pint = do
  whitespace
  n <- many1 digit
  whitespace
  return $ EIntLit (read n)

--BOOL OPS--

opb  = try band <|> try bor <|> opb'
opb' = try numEq <|> try numLt <|> try numGt <|> bterm
bterm = try base <|> try parenB <|> try pvar
  where base = try bRight <|> try bWrong

bRight :: Parser Exp
bRight = do
  whitespace
  string "right"
  whitespace
  return $ EBoolLit True

bWrong :: Parser Exp
bWrong = do
  whitespace
  string "wrong"
  whitespace
  return $ EBoolLit False

--VARS--

pvar :: Parser Exp
pvar = do
  whitespace
  v <- many1 letter
  whitespace
  return $ EVar v

parseExp :: String -> Either ParseError Prog
parseExp src = parse (many1 stmt <* eof) "" src


--pbool -> int comp int | bool op pbool | bool
--cond  -> "if" pbool "then" opa "else" opa

main :: IO ()
main = do
  file <- getContents
  --putStrLn file
  {-case parseExp file of
    Left p -> return "error"
    Right e -> do
      return $ concat $ map show e-}
  let ((_,_),prt) = stepProg [] [] (parseExp file)
  --let a = parseExp file
  putStr $ concat (map (++ "\n") prt)
