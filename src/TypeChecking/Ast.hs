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
import HList
import Language.Haskell.TH hiding (Type, Lit, Pat)

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
    EMatch :: Expr a -> [Alt a b] -> Expr b
data SomeExpr where
    SomeExpr :: SProgType t -> Expr t -> SomeExpr

data Alt :: TProgType -> TProgType -> Type where
    Alt :: Pat p -> Expr e -> Alt p e
data SomeAlt where
    SomeAlt :: SProgType p -> SProgType e -> Alt p e -> SomeAlt

data Pat :: TProgType -> Type where
    PVar :: SProgType t -> Text -> Pat t
    PLit :: Lit a -> Pat a
    PCon :: SEnvT e -> SText s -> SVarWitness w -> HList (HListFromT (GetType s (EVars e) w)) -> Pat (GetType s (EVars e) w)
data SomePat where
    SomePat :: SProgType t -> Pat t -> SomePat

data LetDecl :: TProgType -> Type where
    LetDecl :: Text -> Expr t -> LetDecl t


data TopDef s :: Type where
    DefLet  :: LetDecl t -> TopDef s
    DefData :: Text -> [DataCon s] -> TopDef s

type family HListFromT (t::TProgType) :: [Type] where
    HListFromT (arg :-> tail) = Pat arg : (HListFromT tail)
    HListFromT _ = '[]

-- type family TLength (t :: TProgType) :: [Type] where
--     TLength (a :-> b) = (Pat)
