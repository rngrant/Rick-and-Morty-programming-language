module Optimizations where

import Control.Monad
import CodeGen


-- maps changing association over the multiverse.
optimizeMulti :: Multi -> Multi
optimizeMulti [] = []
optimizeMulti ((str,progs):ms) =
                 (str, fmap (optimize optimizations) progs):optimizeMulti ms

-- maps AST optimizations over statements
optimize :: (Exp -> Exp) -> Stmt -> Stmt
optimize f (SDecl pos str e)      = SDecl pos str (f e)
optimize f (SWhile pos e stmlist) = SWhile pos (f e) (fmap (optimize f) stmlist)
optimize f (SIf pos e sl1 sl2)    = SIf pos (f e) (fmap (optimize f) sl1) (fmap (optimize f) sl2)
optimize _ (SPrint pos str)       = SPrint pos str
optimize _ (SPortal pos str)      = SPortal pos str

-- composition of all AST optimizations
optimizations :: Exp -> Exp
optimizations = andTrue . orFalse . mulOne . addZero . changeAssoc

-- Simple AST optimization for additive identity
-- e + 0 = e, 0 + e = e, e - 0 = e
addZero :: Exp -> Exp
addZero (EBin Add e (EIntLit 0)) = e
addZero (EBin Add (EIntLit 0) e) = e
addZero (EBin Sub e (EIntLit 0)) = e
addZero e                        = e

-- Simple AST optimization for multiplicative identity
-- 1 * e = e, e * 1 = e, e / 1 = e
mulOne :: Exp -> Exp
mulOne (EBin Mul e (EIntLit 1)) = e
mulOne (EBin Mul (EIntLit 1) e) = e
mulOne (EBin Div e (EIntLit 1)) = e
mulOne e                        = e

-- b && True = b for all b
andTrue :: Exp -> Exp
andTrue (EBin And e (EBoolLit True)) = e
andTrue (EBin And (EBoolLit True) e) = e
andTrue e                            = e

-- b || False = b for all b
orFalse :: Exp -> Exp
orFalse (EBin Or e (EBoolLit False)) = e
orFalse (EBin Or (EBoolLit False) e) = e
orFalse e                            = e

-- Changes an Expresion that is right associative
-- into one which is left associative
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
changeAssoc (EVar str)     =  (EVar str)
changeAssoc (EBin op e1 e2) = (EBin op (changeAssoc e1) (changeAssoc e2))
