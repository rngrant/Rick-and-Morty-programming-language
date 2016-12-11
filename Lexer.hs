module Lexer where

import Control.Monad 
import Text.Parsec hiding (crlf)
import Text.Parsec.String
import Control.Applicative ((<*))
import CodeGen
import Debug.Trace

--Parses arithmetic operations within parentheses
parenA = do
  whitespace
  void $ string "you gotta"
  whitespace
  e <- op3
  whitespace
  void $ string "Morty"
  whitespace
  return (EParens e)

--Parses boolean operation within parentheses
parenB = do
  whitespace
  void $ string "you gotta"
  whitespace
  e <- op1
  whitespace
  void $ string "Morty"
  whitespace
  return (EParens e)

--Clears whitespace
whitespace = void $ many $ oneOf" \t\n"

--Clears newlines
crlf = many $ oneOf "\n"

--Parses multiplication
pmul = do
  whitespace
  e1 <- pterm
  whitespace
  string "times"
  whitespace
  e2 <- op4
  whitespace
  return $ EBin Mul e1 e2

--Parses division
pdiv = do
  whitespace
  e1 <- pterm
  whitespace
  string "divided by"
  whitespace
  e2 <- op4
  whitespace
  return $ EBin Div e1 e2

--Parses modulo
pmod = do
  whitespace
  e1 <- pterm
  whitespace
  string "mod"
  whitespace
  e2 <- op4
  whitespace
  return $ EBin Mod e1 e2

--Parses addition
padd = do
  whitespace
  e1 <- op4
  whitespace
  string "plus"
  whitespace
  e2 <- op3
  whitespace
  return $ EBin Add e1 e2

--Parses subtraction
psub = do
  whitespace
  e1 <- op4
  whitespace
  string "minus"
  whitespace
  e2 <- op3
  whitespace
  return $ EBin Sub e1 e2

--BOOLEAN OPERATOR PARSER--

--Parses and
band = do
  whitespace
  e1 <- op2
  whitespace
  string "and"
  whitespace
  e2 <- op1
  whitespace
  return $ EBin And e1 e2

--Parses or
bor = do
  whitespace
  e1 <- op2
  whitespace
  string "or"
  whitespace
  e2 <- op1
  whitespace
  return $ EBin Or e1 e2

--Parses numeric equality
numEq = do
  whitespace
  e1 <- op3
  whitespace
  string "is the same as"
  e2 <- op3
  whitespace
  return $ EBin Equals e1 e2

--Parses numeric less than
numLt = do
  whitespace
  e1 <- op3
  whitespace
  string "is less than"
  e2 <- op3
  whitespace
  return $ EBin LessThan e1 e2

--Parses numeric greater than
numGt = do
  whitespace
  e1 <- op3
  whitespace
  string "is greater than"
  e2 <- op3
  whitespace
  return $ EBin LessThan e1 e2

--STATEMENTS--

--Parses if statements
sIf = do
  whitespace
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
  return $ SIf e1 s1 s2

--Parses variable declaration
sDec = do
  whitespace
  s <- many1 letter
  whitespace
  string "means"
  whitespace
  e <- expr
  whitespace
  return $ SDecl s e

--Parses print statements
sPrint = do
  whitespace
  string "show me"
  whitespace
  s <- many1 letter
  whitespace
  return $ SPrint s

--Parses while statements
sWhile = do
  whitespace
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
  return $ SWhile e s

--Parses portal statements for multithreaded jumps
sPortal = do
  whitespace
  string "lets grab our"
  whitespace
  u <- many1 letter
  whitespace
  string "and portal out of here"
  whitespace
  return $ SPortal u


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
op2 = try numEq <|> try numLt <|> try numGt <|> op3
op3 = try padd <|> try psub <|> op4
op4 = try pmul <|> try pdiv <|> try pmod  <|> pterm
pterm = try base <|> try parenA <|> try parenB <|> try pvar
  where base = pint <|> try bRight <|> try bWrong

pint :: Parser Exp
pint = do
  whitespace
  n <- many1 digit
  whitespace
  return $ EIntLit (read n)

--BOOL OPS--

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

--Calls parse on src expecting at least one univ term
parseExp :: String -> Either ParseError Multi
parseExp src = parse (many1 uParse <* eof) "" src

testParseExp :: String -> Either ParseError Exp
testParseExp src = parse (expr <* eof) "" src

changeAssocStmt :: Stmt -> Stmt
changeAssocStmt (SDecl str e) =  (SDecl str (changeAssoc e))
changeAssocStmt (SWhile e stmlist) =
                (SWhile (changeAssoc e)
                (map changeAssocStmt stmlist))
changeAssocStmt (SIf e stmlist1 stmlist2) =
                (SIf (changeAssoc e)
                (map changeAssocStmt stmlist1)
                (map changeAssocStmt stmlist2))
changeAssocStmt (SPrint str) = (SPrint str)


changeAssoc :: Exp -> Exp
changeAssoc (EIntLit n) = (EIntLit n)
changeAssoc (EBoolLit b) = (EBoolLit b)
changeAssoc (EUOp Neg e) = (EUOp Neg (changeAssoc e))
changeAssoc (EUOp Not e) = (EUOp Not (changeAssoc e))
changeAssoc (EParens  e) = (EParens (changeAssoc e))
changeAssoc (EBin Add e1 (EBin Add e2 e3)) =
            changeAssoc (EBin Add (EBin Add e1 e2) e3)
changeAssoc (EBin Add e1 (EBin Sub e2 e3)) =
            changeAssoc (EBin Sub (EBin Add e1 e2) e3)
changeAssoc (EBin Sub e1 (EBin Add e2 e3)) =
            changeAssoc (EBin Add (EBin Sub e1 e2) e3)
changeAssoc (EBin Sub e1 (EBin Sub e2 e3)) =
            changeAssoc (EBin Sub (EBin Sub e1 e2) e3)
changeAssoc (EBin Mul e1 (EBin Mul e2 e3)) =
            changeAssoc (EBin Mul (EBin Mul e1 e2) e3)
changeAssoc (EBin Mul e1 (EBin Div e2 e3)) =
            changeAssoc (EBin Div (EBin Mul e1 e2) e3)
changeAssoc (EBin Mul e1 (EBin Mod e2 e3)) =
            changeAssoc (EBin Mod (EBin Mul e1 e2) e3)
changeAssoc (EBin Div e1 (EBin Mul e2 e3)) =
            changeAssoc (EBin Mul (EBin Div e1 e2) e3)
changeAssoc (EBin Div e1 (EBin Div e2 e3)) =
            changeAssoc (EBin Div (EBin Div e1 e2) e3)
changeAssoc (EBin Div e1 (EBin Mod e2 e3)) =
            changeAssoc (EBin Mod (EBin Div e1 e2) e3)
changeAssoc (EBin Mod e1 (EBin Mul e2 e3)) =
            changeAssoc (EBin Mul (EBin Mod e1 e2) e3)
changeAssoc (EBin Mod e1 (EBin Div e2 e3)) =
            changeAssoc (EBin Div (EBin Mod e1 e2) e3)
changeAssoc (EBin Mod e1 (EBin Mod e2 e3)) =
            changeAssoc (EBin Mod (EBin Mod e1 e2) e3)
changeAssoc (EIf cond e1 e2)= (EIf (changeAssoc cond)
                                  (changeAssoc e1)
                                  (changeAssoc e2))
changeAssoc (EBin op e1 e2) = (EBin op (changeAssoc e1) (changeAssoc e2))

--RUN FUNCTION: takes the text of an input file, parses, and runs stepUni on the output parse tree
main :: IO ()
main = do
  file <- getContents
  let ((_,_),prt) = stepUni [] [] (parseExp file)
  putStr $ concat (map (++ "\n") prt)

--DEBUG FUNCTION: Returns parse tree of given input file
{-
main :: IO String
main = do
  file <- getContents
  case parseExp file of
    Left p -> return $ show p
    Right e -> do
      return $ concat $ map show e
-}
