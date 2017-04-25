{-# LANGUAGE GADTs #-}

{-Base code provide by Charlie Curtsinger-}

module CodeGen where

import Text.Parsec
import Text.Parsec.String
import Control.Monad.Except

data BOp where
  Add :: BOp
  Sub :: BOp
  Mul :: BOp
  Div :: BOp
  Mod :: BOp
  Equals :: BOp
  GreaterThan :: BOp
  LessThan :: BOp
  And :: BOp
  Or :: BOp
  deriving (Eq)

-- Errors

data Exception = NumArgs Integer [Value]
               | TypeMismatch String Value
               | Parser ParseError
               | BadSpecialForm String Value

showError :: Exception -> String
showError (NumArgs x vals)     = "Expected " ++ show x ++ ", found" ++ show vals
showError (TypeMismatch e f)   = "Invalid type: expected " ++ e ++ ", found " ++ show f
showError (Parser p)           = "Parse error at " ++ show p
showError (BadSpecialForm s v) = s ++ ": " ++ show v

instance Show Exception where show = showError

type ThrowsError = Either Exception

-- Error helpers

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val



instance Show BOp where
  show Add = "Add"
  show Sub = "Sub"
  show Mul = "Mul"
  show Div = "Div"
  show Mod = "Mod"
  show Equals = "Equals"
  show GreaterThan = "GreaterThan"
  show LessThan = "LessThan"
  show And = "And"
  show Or = "Or"

--Unary operations not currently implimented due to time constraints
data UOp where
  Neg :: UOp
  Not :: UOp
  deriving (Eq)

instance Show UOp where
  show Neg = "Cronenberg"
  show Not = "Not"

data Exp where
  EIntLit :: Int -> Exp
  EBoolLit :: Bool -> Exp
  EUOp :: UOp -> Exp -> Exp
  EBin :: BOp -> Exp -> Exp -> Exp
  EIf :: Exp -> Exp -> Exp -> Exp
  EParens :: Exp -> Exp
  EVar :: String -> Exp
  deriving (Eq, Show)

--Basically the pretty print function. This is a debug function.
say :: Exp -> String
say (EIntLit n) = show n
say (EBoolLit b) = show b
say (EUOp op e) = "YouGotta " ++ (show op) ++ (say e) ++ " Morty"
say (EBin op e1 e2) = "YouGotta " ++ (say e1) ++ (show op) ++ (say e2) ++ " Morty"
say (EIf cond e1 e2) = "If " ++ (say cond) ++ " then " ++ (say e1) ++ " otherwise " ++ (say e2)

data Value where
  VInt :: Int -> Value
  VBool :: Bool -> Value
  deriving (Eq, Show)

data Stmt where
  SDecl :: String -> Exp -> Stmt
  SWhile :: Exp -> [Stmt] -> Stmt
  SIf :: Exp -> [Stmt] -> [Stmt] -> Stmt
  SPrint :: String -> Stmt
  SPortal :: String -> Stmt
  deriving (Eq, Show)

type Prog = [Stmt]
type Env = [(String, Value)]
type Block = (String, Prog)
type Multi = [Block]
type Print = [String]

--Evaluates Integer binary operations at the base level
evalIntBOp :: (Int -> Int -> Int) -> (Maybe Value) -> (Maybe Value) -> (Maybe Value)
evalIntBOp op v1 v2 = case (v1, v2) of
  (Just (VInt n1), Just (VInt n2)) -> Just . VInt $ op n1 n2
  _ -> Nothing

--Evaluates Boolean binary expressions at the base level
evalBoolBOp :: (Bool -> Bool -> Bool) -> (Maybe Value) -> (Maybe Value) -> (Maybe Value)
evalBoolBOp op v1 v2 = case (v1, v2) of
  (Just (VBool b1), Just (VBool b2)) -> Just . VBool $ op b1 b2
  _ -> Nothing

--Evaluates Integer Boolean binary expressions at the base level
evalIntBoolBOp :: (Int -> Int -> Bool) -> (Maybe Value) -> (Maybe Value) -> (Maybe Value)
evalIntBoolBOp op v1 v2 = case (v1, v2) of
  (Just (VInt n1), Just (VInt n2)) -> Just . VBool $ op n1 n2
  _ -> Nothing

--eval function
--Takes the current variable environment and an expression and returns a Maybe Value
--Uses pattern matching to match the given expression in order to subevaluate the parameters of the expression
--At the base level, this function calls evalIntBOp, evalBoolBOp, and evalIntBoolBOp to eval base expressions
eval :: Env -> Exp -> Maybe Value
eval env (EVar x) = lookup x env
eval env (EIntLit n) = Just $ VInt n
eval env (EBoolLit b) = Just $ VBool b
eval env (EUOp Neg e) = case eval env e of
  Just (VInt n) -> Just $ VInt (-n)
  _ -> Nothing
eval env (EUOp Not e) = case eval env e of
  Just (VBool b) -> Just . VBool $ not b
  _ -> Nothing
eval env (EBin Add e1 e2) = evalIntBOp (+) (eval env e1) (eval env e2)
eval env (EBin Sub e1 e2) = evalIntBOp (-) (eval env e1) (eval env e2)
eval env (EBin Mul e1 e2) = evalIntBOp (*) (eval env e1) (eval env e2)
eval env (EBin Div e1 e2) = evalIntBOp div (eval env e1) (eval env e2)
eval env (EBin Mod e1 e2) = evalIntBOp mod (eval env e1) (eval env e2)
eval env (EBin And e1 e2) = evalBoolBOp (&&) (eval env e1) (eval env e2)
eval env (EBin Or e1 e2) = evalBoolBOp (||) (eval env e1) (eval env e2)
eval env (EBin Equals e1 e2) = evalIntBoolBOp (==) (eval env e1) (eval env e2)
eval env (EBin LessThan e1 e2) = evalIntBoolBOp (<) (eval env e1) (eval env e2)
eval env (EBin GreaterThan e1 e2) = evalIntBoolBOp (>) (eval env e1) (eval env e2)
eval env (EIf cond e1 e2) = case eval env cond of
  Just (VBool True) -> eval env e1
  Just (VBool False) -> eval env e2
  _ -> Nothing
eval env (EParens e) = eval env e


data Typ where
  TInt :: Typ
  TBool :: Typ
  deriving (Eq, Show)

--Exec function
--Takes the entire multiverse, the current variable environment, the current print buffer, and a statement to execute and returns ((A new variable environment, a list of statements to be added to the execution list), any new strings to be added to the print buffer).
--Runs via pattern matching on the type of statement being evaluated.
exec :: Multi -> Env -> Print -> Stmt -> ((Env, [Stmt]), Print)

--Variable Declaration
--Checks the given String s in the environment. If s is found, updates the value of s with the executed value of e. If s was not found, adds the ordered pair (s, eval env e).
exec _ env _ (SDecl s e) =
  case eval env e of
    Just val  ->
      case lookup s env of
        Just v  -> (((filter (\x -> fst x /= s) env) ++ [(s, val)], []),[])
        Nothing -> (((env ++ [(s , val)]), []),[])
    Nothing -> error "You really fucked it up here: Unable to evaluate passed expression"

--While Loops
--Checks if the eval of e is True or False.
----If e == False, returns the same environment, returns no new statements, returns no new prints
----If e == True, returns the same environment, returns s with the input While statement appended to the end, returns no new prints
exec _ env _ (SWhile e s) =
  case eval env e of
    Just (VBool True) -> ((env, (s ++ [SWhile e s])),[])
    Just (VBool False) -> ((env, []),[])
    Nothing -> error "You really fucked it up here: Passed expression not resolved to boolean"

--Prints Statements
--Will only print variables.
--Returns the same environment, returns no new statements, returns the value of the variable s to the print buffer
exec _ env prt (SPrint s) =
  case lookup s env of
    Just v -> ((env, []), [show v])
    Nothing -> error "You really fucked it up here: Variable not found"

--If Statements
--Evaluates the given boolean expression e
----If e == False, returns the same environment, returns the false branch of the if statement, returns no new prints
----If e == True , returns the same environment, returns the true  branch of the if statement, returns no new prints
exec _ env _ (SIf e s1 s2) =
  case eval env e of
    Just (VBool True)  -> ((env, s1),[])
    Just (VBool False) -> ((env, s2),[])
    _          -> error "You really fucked it up here: Unable to evaluate given boolean"

--Portal Statements
--This function searches the multiverse for an ordered pair starting with the passed string. If the string is found, the program paired with that string is returned. No environment updates or print updates
exec m env _ (SPortal u) =
  case lookup u m of
    Just p -> ((env, p), [])
    _ -> error "You really fucked it up here: Universe not found"

--stepProg takes the entire multiverse, the current environment, the current print buffer, and the list programs currently being executed and returns the resulting ((Enviornment, Program), and Print Buffer)
--Uses pattern matching on the Program list to determine action
--Iterates through the program list executing one statement from each program before executing the next statement.
stepProg :: Multi -> Env -> Print -> [Prog] -> ((Env, Prog), Print)

--There are no more statements to execute. Return the current environment and print buffer. (End execution)
stepProg _ env prt [] = ((env, []),prt)

--The current statement is a Portal statement. Take the programatic output of this and append it to the end of the program list as its own, independent program. Append tail of the 'current' program to the end of the program list
stepProg m env prt (((SPortal u):prog):xs) =
  let ((_, prog'), _) = exec m env prt (SPortal u)
    in stepProg m env prt (xs ++ ([prog'] ++ [prog]))

--The current program contains no more statements. End execution of this program and run stepProg on the rest of the program lsit.
stepProg m env prt (([]):xs) = stepProg m env prt xs

--The Base case.
--Take the first statement from the current program and execute it. Append the programatic output to the front of the current program and move that program to the end of the program list.
stepProg m env prt ((s:prog):xs) =
  let ((env', prog'), prt') = exec m env prt s
    in stepProg m env' (prt ++ prt') (xs ++ ([prog' ++ prog]))

--stepUni [Called from main() in Lexer.hs to execute Lexed parse tree].
--takes an environment (initialized to []), a print buffer (initialized to []), and an Either ParseError Multi and returns ((The current environment, the current program list (should be [] when finished)), the Print Buffer)
stepUni :: Env -> Print -> Either Exception Multi -> ((Env, Prog), Print)

--When there was a parse error in lexing the multiverse, return an error
stepUni _ _ (Left e) = error "Universe parse error"

--If there was no universe declared, return an error
stepUni env prt (Right []) = error "You have destroyed the multiverse"

--Run stepProg on the first universe declared in the given file.
stepUni env prt (Right (s:block)) = stepProg (s:block) env prt [(snd s)]
