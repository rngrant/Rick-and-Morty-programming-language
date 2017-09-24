module REPL where
import Control.Monad
import Text.Parsec
import CodeGen
import Lexer
import System.IO
import System.Environment
import System.FilePath.Posix


class Eval a where
  evaluate :: a -> String

instance Eval Stmt where
  evaluate s = show $ exec [] [] [] s 

instance Eval Exp where
  evaluate e = show $ safeEval [] e

runREPL :: IO ()
runREPL = do
  putStrLn "REPL currently only supports expressions: "
  until_ (== "quit") (readPrompt "GRAMPA>>> ") evalAndPrint



flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- THIS WONT WORK YET
-- NEED TO THINK ABOUT HOW TO PARSE/EVAL FROM REPL
-- PROBABLY NEED TO PERSIST ENV, ETC
evalString :: String -> IO String
evalString s = case parse expr "" s of
  Left p  -> return $ show p
  Right e -> case safeEval [] e of
    Nothing -> return "Evaluation error"
    Just v  -> return $ show v

evalAndPrint :: String -> IO ()
evalAndPrint s = evalString s >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

--RUN FUNCTION: takes an input file, checks file extension, parses, and runs stepUni on the output parse tree
--  If no arguments, starts REPL
main :: IO ()
main = do
  args <- getArgs
  case length args of
    0    -> runREPL
    _    -> compile args


compile :: [String] -> IO ()
compile fs = do  
  grmFiles <- return $ filter (\f -> (takeExtension f) == ".grm") fs
  let file = case grmFiles of
               (x:xs) -> x
               []    -> error $ "Please pass valid GRAMPA file"
  handle <- openFile file  ReadMode
  f <- hGetContents handle
  let ((_,_),prt) = stepUni [] [] (parseExp f)
  putStr $ concat (map (++ "\n") prt)
  hClose handle

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
