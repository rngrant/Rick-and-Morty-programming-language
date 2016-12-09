{-# LANGUAGE GADTs #-}

{-Base code provide by Charlie Curtsinger-}

module CodeGen where

import Text.Parsec
import Text.Parsec.String


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

evalIntBOp :: (Int -> Int -> Int) -> (Maybe Value) -> (Maybe Value) -> (Maybe Value)
evalIntBOp op v1 v2 = case (v1, v2) of
  (Just (VInt n1), Just (VInt n2)) -> Just $ VInt $ op n1 n2
  _ -> Nothing
  
evalBoolBOp :: (Bool -> Bool -> Bool) -> (Maybe Value) -> (Maybe Value) -> (Maybe Value)
evalBoolBOp op v1 v2 = case (v1, v2) of
  (Just (VBool b1), Just (VBool b2)) -> Just $ VBool $ op b1 b2
  _ -> Nothing

evalIntBoolBOp :: (Int -> Int -> Bool) -> (Maybe Value) -> (Maybe Value) -> (Maybe Value)
evalIntBoolBOp op v1 v2 = case (v1, v2) of
  (Just (VInt n1), Just (VInt n2)) -> Just $ VBool $ op n1 n2
  _ -> Nothing

eval :: Env -> Exp -> Maybe Value
eval env (EVar x) = lookup x env
eval env (EIntLit n) = Just $ VInt n
eval env (EBoolLit b) = Just $ VBool b
eval env (EUOp Neg e) = case eval env e of
  Just (VInt n) -> Just $ VInt (-n)
  _ -> Nothing
eval env (EUOp Not e) = case eval env e of
  Just (VBool b) -> Just $ VBool $ not b
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

expectTypes :: Typ -> Exp -> Typ -> Exp -> Typ -> Maybe Typ
expectTypes t1 e1 t2 e2 result = case (typecheck e1, typecheck e2) of
  (Just e1t, Just e2t) -> if e1t == t1 && e2t == t2
    then Just result
    else Nothing
  _ -> Nothing

typecheck :: Exp -> Maybe Typ
typecheck (EParens e) = typecheck e
typecheck (EIntLit n) = Just TInt
typecheck (EBoolLit b) = Just TBool
typecheck (EUOp Not b) =
  case typecheck b of
    Just TBool -> Just TBool
    _ -> Nothing
typecheck (EUOp Neg n) =
  case typecheck n of
    Just TInt -> Just TInt
    _ -> Nothing
typecheck (EBin Add e1 e2) = expectTypes TInt e1 TInt e2 TInt
typecheck (EBin Sub e1 e2) = expectTypes TInt e1 TInt e2 TInt
typecheck (EBin Mul e1 e2) = expectTypes TInt e1 TInt e2 TInt
typecheck (EBin Div e1 e2) = expectTypes TInt e1 TInt e2 TInt
typecheck (EBin Mod e1 e2) = expectTypes TInt e1 TInt e2 TInt
typecheck (EBin And e1 e2) = expectTypes TBool e1 TBool e2 TBool
typecheck (EBin Or e1 e2) = expectTypes TBool e1 TBool e2 TBool
typecheck (EBin Equals e1 e2) = expectTypes TInt e1 TInt e2 TBool
typecheck (EBin LessThan e1 e2) = expectTypes TInt e1 TInt e2 TBool
typecheck (EBin GreaterThan e1 e2) = expectTypes TInt e1 TInt e2 TBool
typecheck (EIf c e1 e2) =
  case typecheck c of
    Just TBool -> case (typecheck e1, typecheck e2) of
      (Just t1, Just t2) -> if t1 == t2 then Just t1 else Nothing
    _ -> Nothing
--typecheck (SDecl _ e) = typecheck e


printHelp :: Show a => a -> IO ()
printHelp = print

safeEval :: Either ParseError Exp -> Maybe Value
safeEval (Right e) =
  case typecheck e of
    Just _ -> eval [] e
    _      -> Nothing
safeEval (Left e)= Nothing

exec :: Multi -> Env -> Print -> Stmt -> ((Env, [Stmt]), Print)
exec _ env _ (SDecl s e) =
  case eval env e of
    Just val  ->
      case lookup s env of
        Just v  -> (((filter (\x -> fst x /= s) env) ++ [(s, val)], []),[]) 
        Nothing -> (((env ++ [(s , val)]), []),[])
    Nothing -> error "You really fucked it up here: Unable to evaluate passed expression"
exec _ env _ (SWhile e s) =
  case eval env e of
    Just (VBool True) -> ((env, (s ++ [SWhile e s])),[])
    Just (VBool False) -> ((env, []),[])
    Nothing -> error "You really fucked it up here: Passed expression not resolved to boolean"
exec _ env prt (SPrint s) =
  case lookup s env of
    Just v -> ((env, []), [show v])
    Nothing -> error "You really fucked it up here: Variable not found"
      
exec _ env _ (SIf e s1 s2) =
  case eval env e of
    Just (VBool True)  -> ((env, s1),[])
    Just (VBool False) -> ((env, s2),[])
    _          -> error "You really fucked it up here: Unable to evaluate given boolean"
exec m env _ (SPortal u) =
  case lookup u m of
    Just p -> ((env, p), [])
    _ -> error "You really fucked it up here: Universe not found"


stepProg :: Multi -> Env -> Print -> Either ParseError Prog -> ((Env, Prog), Print)
stepProg _ _  _(Left e) = error "Parse error"
stepProg _ env prt (Right []) = ((env, []),prt)
stepProg m env prt (Right ((SPortal u):prog)) =
  let ((_, prog'), _) = exec m env prt (SPortal u)
    in stepProg m env prt (Right (merge prog' prog))
stepProg m env prt (Right (s:prog)) =
  let ((env', prog'), prt') = exec m env prt s
    in stepProg m env' (prt ++ prt') (Right (prog ++ prog'))

stepUni :: Env -> Print -> Either ParseError Multi -> ((Env, Prog), Print)
stepUni _ _ (Left e) = error "Universe Parse error"
stepUni env prt (Right []) = error "You have destroyed the multiverse"
stepUni env prt (Right (s:block)) = stepProg (s:block) env prt (Right (snd s))

--http://stackoverflow.com/questions/3938438/merging-two-lists-in-haskell
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

