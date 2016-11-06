module Lexer where

import Control.Monad 
import Text.Parsec
import Text.Parsec.String
import CodeGen

--import System.IO
--import System.Environment
--import Control.Monad
--import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.Expr
--import Text.ParserCombinators.Parsec.Language
--import qualified Text.ParserCombinators.Parsec.Token as Token

{-data Token where
  TInt :: Int -> Token
  TDouble :: Double -> Token
  TBool :: Bool -> Token
  TAdd :: Token
  TSub :: Token
  TMul :: Token
  TDiv :: Token
  TEquals :: Token
  TGreaterThan :: Token
  TLessThan :: Token -}

parensEN simpExprImpl = do
  whitespace
  void $ string "you gotta"
  whitespace
  e <- simpExprImpl
  whitespace
  void $ string "Morty"
  return (EParens e)

whitespace = void $ many $ oneOf " \n\t"

padd = do
  whitespace
  e1 <- term7
  whitespace
  string "plus"
  whitespace
  e2 <- pint
  whitespace
  return $ EBin Add e1 e2

psub = do
  whitespace
  e1 <- term7
  whitespace
  string "minus"
  whitespace
  e2 <- pint
  whitespace
  return $ EBin Sub e1 e2

pexp = do    --simpleExper7
  e <- term7

  maybeOpSuffix e

  where
    maybeOpSuffix e = 
      try $ addSuffix e <|> subSuffix e <|> return e
        where
          addSuffix e = do
            whitespace
            string "plus"
            whitespace
            e1 <- term7
            maybeOpSuffix (EBin Add e e1)
          subSuffix e = do
            whitespace
            string "minus"
            whitespace
            e1 <- term7
            maybeOpSuffix (EBin Sub e e1)

term7 = pterm pexp

pterm simpleExpr = try $ base <|> parensEN simpleExpr --term
  where base = pint

pint :: Parser Exp
pint = do
  n <- many1 digit
  return $ EIntLit (read n)

parseExp :: String -> Either ParseError Exp
parseExp src = parse term7 "" src

{-main :: IO()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))

symbol :: Parser String
symbol = oneOf ["Plus", "Minus", "Times", "Divided By"]

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

data RickVal = Number Integer



numericBinop :: (Integer -> Integer -> Integer) -> [RickVal] -> RickVal
numericBinop op params = Number $ foldl1 op $ op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                            then 0
                            else fst $ parsed !! 0
unpack

languageDef =
  emptyDef { Token.commentStart   = "{-"
           , Token.commentEnd     = "-}"
           , Token.commentLine    = "--"
           , Token.identStart     = letter
           , Token.identLetter    = alphaNum
           , Token.reservedNames  = [ "If"
                                    , "then"
                                    , "otherwise"
                                    , "true"
                                    , "false"
                                    , "not"
                                    , "and"
                                    , "or"
                                    ]
           , Token.reservedOpNames = [ "Plus", "Minus", "Times", "DividedBy"
                                     , "LessThan", "GreaterThan", "And", "Or"
                                     ,"Not"
                                     ]
           , Token.caseSensitive = True
}

lexer = Token.makeTokenParser languageDef
-}
