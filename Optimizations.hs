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
addZero (EBin _ Add e (EIntLit _ 0)) = e
addZero (EBin _ Add (EIntLit _ 0) e) = e
addZero (EBin _ Sub e (EIntLit _ 0)) = e
addZero e                            = e

-- Simple AST optimization for multiplicative identity
-- 1 * e = e, e * 1 = e, e / 1 = e
mulOne :: Exp -> Exp
mulOne (EBin _ Mul e (EIntLit _ 1)) = e
mulOne (EBin _ Mul (EIntLit _ 1) e) = e
mulOne (EBin _ Div e (EIntLit _ 1)) = e
mulOne e                            = e

-- b && True = b for all b
andTrue :: Exp -> Exp
andTrue (EBin _ And e (EBoolLit _ True)) = e
andTrue (EBin _ And (EBoolLit _ True) e) = e
andTrue e                                = e

-- b || False = b for all b
orFalse :: Exp -> Exp
orFalse (EBin _ Or e (EBoolLit _ False)) = e
orFalse (EBin _ Or (EBoolLit _ False) e) = e
orFalse e                                = e

-- Changes an Expresion that is right associative
-- into one which is left associative
changeAssoc :: Exp -> Exp
changeAssoc (EIntLit p n) = (EIntLit p n)
changeAssoc (EBoolLit p b) = (EBoolLit p b)
changeAssoc (EUOp p Neg e) = (EUOp p Neg (changeAssoc e))
changeAssoc (EUOp p Not e) = (EUOp p Not (changeAssoc e))
changeAssoc (EParens p e) = (EParens p (changeAssoc e))
changeAssoc (EBin p1 Add e1 (EBin p2 Add e2 e3)) =
            changeAssoc (EBin p2 Add (EBin p1 Add e1 e2) e3)
changeAssoc (EBin p1 Add e1 (EBin p2 Sub e2 e3)) =
            changeAssoc (EBin p2 Sub (EBin p1 Add e1 e2) e3)
changeAssoc (EBin p1 Sub e1 (EBin p2 Add e2 e3)) =
            changeAssoc (EBin p2 Add (EBin p1 Sub e1 e2) e3)
changeAssoc (EBin p1 Sub e1 (EBin p2 Sub e2 e3)) =
            changeAssoc (EBin p2 Sub (EBin p1 Sub e1 e2) e3)
changeAssoc (EBin p1 Mul e1 (EBin p2 Mul e2 e3)) =
            changeAssoc (EBin p2 Mul (EBin p1 Mul e1 e2) e3)
changeAssoc (EBin p1 Mul e1 (EBin p2 Div e2 e3)) =
            changeAssoc (EBin p2 Div (EBin p1 Mul e1 e2) e3)
changeAssoc (EBin p1 Mul e1 (EBin p2 Mod e2 e3)) =
            changeAssoc (EBin p2 Mod (EBin p1 Mul e1 e2) e3)
changeAssoc (EBin p1 Div e1 (EBin p2 Mul e2 e3)) =
            changeAssoc (EBin p2 Mul (EBin p1 Div e1 e2) e3)
changeAssoc (EBin p1 Div e1 (EBin p2 Div e2 e3)) =
            changeAssoc (EBin p2 Div (EBin p1 Div e1 e2) e3)
changeAssoc (EBin p1 Div e1 (EBin p2 Mod e2 e3)) =
            changeAssoc (EBin p2 Mod (EBin p1 Div e1 e2) e3)
changeAssoc (EBin p1 Mod e1 (EBin p2 Mul e2 e3)) =
            changeAssoc (EBin p2 Mul (EBin p1 Mod e1 e2) e3)
changeAssoc (EBin p1 Mod e1 (EBin p2 Div e2 e3)) =
            changeAssoc (EBin p2 Div (EBin p1 Mod e1 e2) e3)
changeAssoc (EBin p1 Mod e1 (EBin p2 Mod e2 e3)) =
            changeAssoc (EBin p2 Mod (EBin p1 Mod e1 e2) e3)
changeAssoc (EIf p cond e1 e2)= (EIf p (changeAssoc cond)
                                   (changeAssoc e1)
                                   (changeAssoc e2))
changeAssoc (EVar p str)     =  (EVar p str)
changeAssoc (EBin p op e1 e2) = (EBin p op (changeAssoc e1) (changeAssoc e2))
