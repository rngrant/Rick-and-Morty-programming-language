import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

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

