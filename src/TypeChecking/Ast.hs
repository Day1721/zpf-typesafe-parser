{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module TypeChecking.Ast where

import Data.Kind
import Basic.Single
import GHC.TypeLits
import TypeChecking.Basic

data Lit :: (ProgType Symbol) -> Type where
    LUnit ::           Lit PUnit
    LInt  :: Int    -> Lit PInt
    LStr  :: String -> Lit PStr
data SomeLit where
    SomeLit :: SProgType t -> Lit t -> SomeLit

data Expr :: (ProgType Symbol) -> Type where
    ELit :: Lit a -> Expr a
    EVar :: SEnvT e -> SText s -> SVarWitness w -> Expr (GetType s (EVars e) w)
    EApp :: Expr (a :-> b) -> Expr a -> Expr b
    EAbs :: SText s -> SProgType t -> Expr r -> Expr (t :-> r)
    ELet :: LetDecl t -> Expr t' -> Expr t'
data SomeExpr where
    SomeExpr :: SProgType t -> Expr t -> SomeExpr

data LetDecl :: (ProgType Symbol) -> Type where
    LetDecl :: Text -> Expr t -> LetDecl t

data DataCon :: Type where
    DataCon :: Text -> [ProgType Text] -> DataCon

data TopDef :: Type where
    TDLet  :: LetDecl t -> TopDef
    TDData :: Text -> [DataCon] -> TopDef