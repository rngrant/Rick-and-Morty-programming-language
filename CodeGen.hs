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
  EIntLit :: SourcePos -> Int -> Exp
  EBoolLit :: SourcePos -> Bool -> Exp
  EUOp :: SourcePos -> UOp -> Exp -> Exp
  EBin :: SourcePos -> BOp -> Exp -> Exp -> Exp
  EIf :: SourcePos -> Exp -> Exp -> Exp -> Exp
  EParens :: SourcePos -> Exp -> Exp
  EVar :: SourcePos -> String -> Exp
  deriving (Eq, Show)

--Basically the pretty print function. 
say :: Exp -> String
say (EIntLit _ n) = show n
say (EBoolLit _ b) = show b
say (EUOp _ op e) = "YouGotta " ++ (show op) ++ (say e) ++ " Morty"
say (EBin _ op e1 e2) = "YouGotta " ++ (say e1) ++ (show op) ++ (say e2) ++ " Morty"
say (EIf _ cond e1 e2) = "If " ++ (say cond) ++ " then " ++ (say e1) ++ " otherwise " ++ (say e2)

data Value where
  VInt :: Int -> Value
  VBool :: Bool -> Value
  deriving (Eq, Show)

data Stmt where
  SDecl :: SourcePos -> String -> Exp -> Stmt
  SWhile :: SourcePos -> Exp -> [Stmt] -> Stmt
  SIf :: SourcePos -> Exp -> [Stmt] -> [Stmt] -> Stmt
  SPrint :: SourcePos -> String -> Stmt
  SPortal :: SourcePos -> String -> Stmt
  deriving (Eq, Show)

type Prog = [Stmt]
type Env = [(String, Value)]
type Block = (String, Prog)
type Multi = [Block]
type Print = [String]

--TODO: reevaluate the `(min p1 p2)` usage -- why?
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
eval env (EVar _ x) = lookup x env
eval env (EIntLit _ n) = Just $ VInt n
eval env (EBoolLit _ b) = Just $ VBool b
eval env (EUOp _ Neg e) = case eval env e of
  Just (VInt n) -> Just $ VInt (-n)
  _ -> Nothing
eval env (EUOp _ Not e) = case eval env e of
  Just (VBool b) -> Just . VBool $ not b
  _ -> Nothing
eval env (EBin _ Add e1 e2) = evalIntBOp (+) (eval env e1) (eval env e2)
eval env (EBin _ Sub e1 e2) = evalIntBOp (-) (eval env e1) (eval env e2)
eval env (EBin _  Mul e1 e2) = evalIntBOp (*) (eval env e1) (eval env e2)
eval env (EBin _ Div e1 e2) = evalIntBOp div (eval env e1) (eval env e2)
eval env (EBin _ Mod e1 e2) = evalIntBOp mod (eval env e1) (eval env e2)
eval env (EBin _ And e1 e2) = evalBoolBOp (&&) (eval env e1) (eval env e2)
eval env (EBin _ Or e1 e2) = evalBoolBOp (||) (eval env e1) (eval env e2)
eval env (EBin _ Equals e1 e2) = evalIntBoolBOp (==) (eval env e1) (eval env e2)
eval env (EBin _ LessThan e1 e2) = evalIntBoolBOp (<) (eval env e1) (eval env e2)
eval env (EBin _ GreaterThan e1 e2) = evalIntBoolBOp (>) (eval env e1) (eval env e2)
eval env (EIf _ cond e1 e2) = case eval env cond of
  Just (VBool True) -> eval env e1
  Just (VBool False) -> eval env e2
  _ -> Nothing
eval env (EParens _ e) = eval env e


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
exec _ env _ (SDecl pos s e) =
  case eval env e of
    Just val  ->
      case lookup s env of
        Just v  -> (((filter (\x -> fst x /= s) env) ++ [(s, val)], []),[])
        Nothing -> (((env ++ [(s , val)]), []),[])
    Nothing -> error $ "You really fucked it up here: Unable to evaluate passed expression " ++ (show pos)

--While Loops
--Checks if the eval of e is True or False.
----If e == False, returns the same environment, returns no new statements, returns no new prints
----If e == True, returns the same environment, returns s with the input While statement appended to the end, returns no new prints
exec _ env _ (SWhile pos e s) =
  case eval env e of
    Just (VBool True) -> ((env, (s ++ [SWhile pos e s])),[])
    Just (VBool False) -> ((env, []),[])
    Nothing -> error $ "You really fucked it up here: Passed expression not resolved to boolean " ++ (show pos)

--Prints Statements
--Will only print variables.
--Returns the same environment, returns no new statements, returns the value of the variable s to the print buffer
exec _ env prt (SPrint pos s) =
  case lookup s env of
    Just v -> ((env, []), [show v])
    Nothing -> error $ "You really fucked it up here: Variable not found " ++ (show pos)

--If Statements
--Evaluates the given boolean expression e
----If e == False, returns the same environment, returns the false branch of the if statement, returns no new prints
----If e == True , returns the same environment, returns the true  branch of the if statement, returns no new prints
exec _ env _ (SIf pos e s1 s2) =
  case eval env e of
    Just (VBool True)  -> ((env, s1),[])
    Just (VBool False) -> ((env, s2),[])
    _          -> error $ "You really fucked it up here: Unable to evaluate given boolean " ++ (show pos)

--Portal Statements
--This function searches the multiverse for an ordered pair starting with the passed string. If the string is found, the program paired with that string is returned. No environment updates or print updates
exec m env _ (SPortal pos u) =
  case lookup u m of
    Just p -> ((env, p), [])
    _ -> error $ "You really fucked it up here: Universe not found " ++ (show pos)

--stepProg takes the entire multiverse, the current environment, the current print buffer, and the list programs currently being executed and returns the resulting ((Enviornment, Program), and Print Buffer)
--Uses pattern matching on the Program list to determine action
--Iterates through the program list executing one statement from each program before executing the next statement.
stepProg :: Multi -> Env -> Print -> [Prog] -> ((Env, Prog), Print)

--There are no more statements to execute. Return the current environment and print buffer. (End execution)
stepProg _ env prt [] = ((env, []),prt)

--The current statement is a Portal statement. Take the programatic output of this and append it to the end of the program list as its own, independent program. Append tail of the 'current' program to the end of the program list
stepProg m env prt (((SPortal pos u):prog):xs) =
  let ((_, prog'), _) = exec m env prt (SPortal pos u)
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
