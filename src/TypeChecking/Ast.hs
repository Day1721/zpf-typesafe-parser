{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module TypeChecking.Ast where

import Data.Kind
import Basic.Single
import TypeChecking.Basic

data Lit :: ProgType -> Type where
    LInt :: Int -> Lit PInt
data SomeLit where
    SomeLit :: SProgType t -> Lit t -> SomeLit

data Expr :: ProgType -> Type where
    ELit :: Lit a -> Expr a
    EVar :: SEnvT e -> SText s -> SVarWitness w -> Expr (GetType s (EVars e) w)
    EApp :: Expr (PFun a b) -> Expr a -> Expr b
    EAbs :: SText s -> SProgType t -> Expr r -> Expr (PFun t r)
data SomeExpr where
    SomeExpr :: SProgType t -> Expr t -> SomeExpr