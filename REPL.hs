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

-- TODO: figure out how to persist Env
evalString :: String -> IO String
evalString s = case parse stmt "" s of
  Left p  -> case parse expr "" s of 
    Left p -> return "Parse error"
    Right e -> case safeEval [] e of
      Nothing -> return "Evalutation error"
      Just v -> return $ show v
  Right s -> return $ unlines $ snd $ stmtExec [] [] [s]
    

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
  grmFiles <- return $ filter (\f -> takeExtension f == ".grm") fs
  let file = case grmFiles of
               (x:xs) -> x
               []    -> error  "Please pass valid GRAMPA file"
  handle <- openFile file  ReadMode
  f <- hGetContents handle
  let ((_,_),prt) = stepUni [] [] (parseExp f)
  putStr $ unlines prt 
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

-- Function for executing a statement within the context of 
--   the REPL: does not support parallelism. Slightly simpler
--   version of execi
stmtExec :: Env -> Print -> Prog -> ((Env, Prog), Print)
stmtExec env p [] = ((env, []), p)
stmtExec env p (SDecl pos str exp:xs) = case safeEval env exp of 
  Just val ->
    case lookup str env of
      Just v -> stmtExec (filter (\x -> fst x /= str) env ++ [(str, val)]) p xs
      Nothing -> stmtExec (env ++ [(str, val)]) p xs
  Nothing -> error $ "You really fucked it up here: Unable to evaluate passed expression " ++ show p


stmtExec env p (SWhile pos exp stmts:xs) = case safeEval env exp of 
  Just (VBool True) -> stmtExec env p (xs ++ stmts)
  Just (VBool False) -> stmtExec env p xs
  Nothing -> error $ "You really fucked it up here: Passed expression not resolved to boolean " ++ show pos

stmtExec env p (SIf pos exp tstmts fstmts:xs) = case safeEval env exp of
  Just (VBool True) -> stmtExec env p (xs ++ tstmts)
  Just (VBool False) -> stmtExec env p (xs ++ fstmts)
  Nothing -> error $ "You really fucked it up here: Unable to evaluate given boolean " ++ show pos

stmtExec env p (SPrint pos str:xs) = case lookup str env of 
  Just v -> stmtExec env (p ++ [(show v)]) xs
  Nothing -> error $  "You really fucked it up here: Variable not found " ++ show pos

stmtExec env p (SPortal pos str:xs) = stmtExec env (p ++ ["REPL does not support parallelism"]) xs
