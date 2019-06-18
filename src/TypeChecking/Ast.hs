{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeChecking.Ast where

import Data.Kind
import Basic.Single
import Basic.TH
import Basic.SingleInstances
import TypeChecking.Basic
import Language.Haskell.TH hiding (Type, Lit)

data Lit :: TProgType -> Type where
    LUnit ::           Lit PUnit
    LInt  :: Int    -> Lit PInt
    LStr  :: String -> Lit PStr
data SomeLit where
    SomeLit :: SProgType t -> Lit t -> SomeLit

data Expr :: TProgType -> Type where
    ELit :: Lit a -> Expr a
    EVar :: SEnvT e -> SText s -> SVarWitness w -> Expr (GetType s (EVars e) w)
    EApp :: Expr (a :-> b) -> Expr a -> Expr b
    EAbs :: SText s -> SProgType t -> Expr r -> Expr (t :-> r)
    ELet :: LetDecl t -> Expr t' -> Expr t'
data SomeExpr where
    SomeExpr :: SProgType t -> Expr t -> SomeExpr

data LetDecl :: TProgType -> Type where
    LetDecl :: Text -> Expr t -> LetDecl t


data TopDef s :: Type where
    DefLet  :: LetDecl t -> TopDef s
    DefData :: Text -> [DataCon s] -> TopDef s
