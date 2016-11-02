{-# LANGUAGE GADTs #-}

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
  show Add = " Plus "
  show Sub = " Minus "
  show Mul = " Times "
  show Div = " DividedBy "
  show Mod = " Mod "
  show Equals = " Equals "
  show GreaterThan = " GreaterThan "
  show LessThan = " LessThan "
  show And = " And "
  show Or = " Or "

data UOp where
  Neg :: UOp
  Not :: UOp
  deriving (Eq)

instance Show UOp where
  show Neg = " Cronenberg "
  show Not = " Not "

data Exp where
  EIntLit :: Int -> Exp
  EBoolLit :: Bool -> Exp
  EUOp :: UOp -> Exp -> Exp
  EBin :: BOp -> Exp -> Exp -> Exp
  EIf :: Exp -> Exp -> Exp -> Exp
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

eval :: Exp -> Maybe Value
eval (EIntLit n) = Just $ VInt n
eval (EBoolLit b) = Just $ VBool b
eval (EUOp Neg e) = case eval e of
  Just (VInt n) -> Just $ VInt (-n)
  _ -> Nothing
eval (EUOp Not e) = case eval e of
  Just (VBool b) -> Just $ VBool $ not b
  _ -> Nothing
eval (EBin Add e1 e2) = evalIntBOp (+) (eval e1) (eval e2)
eval (EBin Sub e1 e2) = evalIntBOp (-) (eval e1) (eval e2)
eval (EBin Mul e1 e2) = evalIntBOp (*) (eval e1) (eval e2)
eval (EBin Div e1 e2) = evalIntBOp div (eval e1) (eval e2)
eval (EBin Mod e1 e2) = evalIntBOp mod (eval e1) (eval e2)
eval (EBin And e1 e2) = evalBoolBOp (&&) (eval e1) (eval e2)
eval (EBin Or e1 e2) = evalBoolBOp (||) (eval e1) (eval e2)
eval (EBin Equals e1 e2) = evalIntBoolBOp (==) (eval e1) (eval e2)
eval (EBin LessThan e1 e2) = evalIntBoolBOp (<) (eval e1) (eval e2)
eval (EBin GreaterThan e1 e2) = evalIntBoolBOp (>) (eval e1) (eval e2)
eval (EIf cond e1 e2) = case eval cond of
  Just (VBool True) -> eval e1
  Just (VBool False) -> eval e2
  _ -> Nothing

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

safeEval :: Exp -> Maybe Value
safeEval e =
  case typecheck e of
    Just _ -> eval e
    _      -> Nothing

