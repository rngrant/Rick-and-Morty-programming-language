module Lexer where
import Control.Monad
import Text.Parsec hiding (crlf)
import Text.Parsec.String
import Control.Applicative ((<*))
import CodeGen
import Debug.Trace
import Optimizations 
import Control.Monad.Except
import System.IO

parenA = do
  whitespace
  p <- getPosition
  void $ string "you gotta"
  whitespace
  e <- op3
  whitespace
  void $ string "Morty"
  whitespace
  return (EParens p e)

--Parses boolean operation within parentheses
parenB = do
  whitespace
  p <- getPosition
  void $ string "you gotta"
  whitespace
  e <- op1
  whitespace
  void $ string "Morty"
  whitespace
  return (EParens p e)

--Clears whitespace
whitespace = void . many $ oneOf" \t\n"

--Clears newlines
crlf = many $ oneOf "\n"

--Parses multiplication
pmul = do
  whitespace
  p <- getPosition
  e1 <- pterm
  whitespace
  string "times"
  whitespace
  e2 <- op4
  whitespace
  return $ EBin p Mul e1 e2

--Parses division
pdiv = do
  whitespace
  p <- getPosition
  e1 <- pterm
  whitespace
  string "divided by"
  whitespace
  e2 <- op4
  whitespace
  return $ EBin p Div e1 e2

--Parses modulo
pmod = do
  whitespace
  p <- getPosition
  e1 <- pterm
  whitespace
  string "mod"
  whitespace
  e2 <- op4
  whitespace
  return $ EBin p Mod e1 e2

--Parses addition
padd = do
  whitespace
  p <- getPosition
  e1 <- op4
  whitespace
  string "plus"
  whitespace
  e2 <- op3
  whitespace
  return $ EBin p Add e1 e2

--Parses subtraction
psub = do
  whitespace
  p <- getPosition
  e1 <- op4
  whitespace
  string "minus"
  whitespace
  e2 <- op3
  whitespace
  return $ EBin p Sub e1 e2

--Parses negative
pneg = do
  whitespace
  p <- getPosition
  string "cronenberg"
  whitespace
  e1 <- op3
  whitespace
  return $ EUOp p Neg e1

--BOOLEAN OPERATOR PARSER--

--Parses and
band = do
  whitespace
  p <- getPosition
  e1 <- op2
  whitespace
  string "and"
  whitespace
  e2 <- op1
  whitespace
  return $ EBin p And e1 e2

--Parses or
bor = do
  whitespace
  p <- getPosition
  e1 <- op2
  whitespace
  string "or"
  whitespace
  e2 <- op1
  whitespace
  return $ EBin p Or e1 e2

--Parses numeric equality
numEq = do
  whitespace
  p <- getPosition
  e1 <- op3
  whitespace
  string "is the same as"
  e2 <- op3
  whitespace
  return $ EBin p Equals e1 e2

--Parses numeric less than
numLt = do
  whitespace
  p <- getPosition
  e1 <- op3
  whitespace
  string "is less than"
  e2 <- op3
  whitespace
  return $ EBin p LessThan e1 e2

--Parses numeric greater than
numGt = do
  whitespace
  p <- getPosition
  e1 <- op3
  whitespace
  string "is greater than"
  e2 <- op3
  whitespace
  return $ EBin p LessThan e1 e2

--Parses not
bnot = do
  whitespace
  p <- getPosition
  string "not "
  whitespace
  e1 <- op1
  whitespace
  return $ EUOp p Not e1

--STATEMENTS--

--Parses if statements
sIf = do
  whitespace
  p <- getPosition
  string "if"
  whitespace
  e1 <- op1
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
  return $ SIf p e1 s1 s2

--Parses variable declaration
sDec = do
  whitespace
  p <- getPosition
  s <- many1 letter
  whitespace
  string "means"
  whitespace
  e <- expr
  whitespace
  return $ SDecl p s e

--Parses print statements
sPrint = do
  whitespace
  p <- getPosition
  string "show me"
  whitespace
  s <- many1 letter
  whitespace
  return $ SPrint p s

--Parses while statements
sWhile = do
  whitespace
  p <- getPosition
  string "while"
  whitespace
  e <- op1
  whitespace
  string "do this for grandpa"
  whitespace
  s <- stmt `endBy` crlf
  whitespace
  string "thanks Summer"
  whitespace
  return $ SWhile p e s

--Parses portal statements for multithreaded jumps
sPortal = do
  whitespace
  p <- getPosition
  string "lets grab our"
  whitespace
  u <- many1 letter
  whitespace
  string "and portal out of here"
  whitespace
  return $ SPortal p u


--UNIVERSE--

--Parses a single universe (ie. a list of statements between a universe declaration and "destroy universe"
uParse = do
  whitespace
  string "universe"
  whitespace
  s <- many1 letter
  whitespace
  b <- stmt `endBy` crlf
  whitespace
  string "destroy universe"
  whitespace
  return $ (s, b)




--GRAMMAR--

--multi -> [univ]
--univ -> [stmt]
--stmt -> if | while | dec | portal | print
--if -> 'if' opb 'then' [stmt] 'otherwise' [stmt]

--expr -> op1

--op1  -> op2 and op1 | op2 or op1  | op2
--op2  -> op3 == op2  | op3 < op2   | op3 > op2  | op3
--op3  -> op4 + op3   | op4 - op3   | op4
--op4  -> pterm * op4 | pterm / op4 | pterm mod op4 | pterm
--pterm -> base | parenA | parenB | pvar
--base -> pint | bRight | bWrong

stmt = try sPortal <|> try sIf <|> try sDec <|> try sPrint <|> try sWhile

expr = try op1

--NUMBER OPS--

op1 = try band <|> try bor <|> op2
op2 = try numEq <|> try numLt <|> try numGt <|> try bnot <|> op3
op3 = try padd <|> try psub <|> op4
op4 = try pmul <|> try pdiv <|> try pmod  <|> pterm
pterm = try base <|> try parenA <|> try parenB <|> try pneg <|>  try pvar
  where base = pint <|> try bRight <|> try bWrong

pint :: Parser Exp
pint = do
  whitespace
  p <- getPosition
  n <- many1 digit
  whitespace
  return $ EIntLit p (read n)

--BOOL OPS--

bRight :: Parser Exp
bRight = do
  whitespace
  p <- getPosition
  string "right"
  whitespace
  return $ EBoolLit p True

bWrong :: Parser Exp
bWrong = do
  whitespace
  p <- getPosition
  string "wrong"
  whitespace
  return $ EBoolLit p False

--VARS--

pvar :: Parser Exp
pvar = do
  whitespace
  p <- getPosition
  v <- many1 letter
  whitespace
  return $ EVar p v

readExpr :: String -> ThrowsError Multi
readExpr src = case parse (many1 uParse <* eof) "" src of
  Left err -> throwError $ Parser err
  Right val -> return val

--Calls parse on src expecting at least one univ term
parseExp :: String -> ThrowsError Multi
parseExp src = fmap optimizeMulti (readExpr src)

-- Code for testing parsed expresions
{-
testParseExp :: String -> Either ParseError Exp
testParseExp src = parse (expr <* eof) "" src
-}
