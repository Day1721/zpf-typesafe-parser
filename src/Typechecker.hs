{-# LANGUAGE GADTs #-} 
{-# LANGUAGE TypeInType #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Typechecker where

import Data.Kind
import Data.Type.Equality
import GHC.TypeLits hiding (Text)
import Basic.Single

data ProgType = PInt | PFun ProgType ProgType

type family LiftProgType t where
    LiftProgType PInt = Int
    LiftProgType (PFun x y) = LiftProgType x -> LiftProgType y

-- environment type
-- s is essentially string, so 'Text' on term level and 'Symbol' on type level
type Vars s = [(s, ProgType)]
data Env s = Env (Vars s)

type instance Demote ProgType = ProgType
instance Single ProgType where
    data instance Singl (e :: Demote ProgType) where
        SPInt :: SProgType PInt
        SPFun :: SProgType a -> SProgType b -> SProgType (PFun a b)
    fromSingl SPInt = PInt
    fromSingl (SPFun x y) = PFun (fromSingl x) (fromSingl y)

    toSingl PInt = SomeSingl SPInt
    toSingl (PFun x y) = x >=> \x ->
        y >=> \y ->
        SomeSingl (SPFun x y)
type SProgType (x :: ProgType) = Singl x

type instance Demote (Env s) = Env (Demote s)
instance Single s => Single (Env s) where
    data instance Singl (e :: Demote (Env s)) where
        SEnv :: SList (l :: [(Demote s, ProgType)]) -> SEnv ('Env l)
    fromSingl (SEnv sl) = Env (fromSingl sl)
    toSingl (Env sl) = sl >=> \sl ->
        SomeSingl $ SEnv sl
type SEnv (e :: Demote (Env s)) = Singl e
type SEnvT (e :: Demote (Env Text)) = SEnv e

data VarWitness (l :: Vars Symbol) (s :: Symbol) where
    Here  :: VarWitness ('(s, pt):t) s
    There :: VarWitness t s -> VarWitness (h:t) s

type instance Demote (VarWitness l s) = VarWitness l s
instance Single (VarWitness l s) where
    data instance Singl (w :: Demote (VarWitness l s)) where
        SHere :: SVarWitness 'Here
        SThere :: SVarWitness w -> SVarWitness ('There w)
    fromSingl SHere = Here
    fromSingl (SThere w) = There (fromSingl w)

    toSingl Here = SomeSingl SHere
    toSingl (There w) = w >=> \w -> SomeSingl (SThere w)
type SVarWitness (w :: Demote (VarWitness l s)) = Singl w

findType :: SText s -> SList l -> Maybe (VarWitness l s)
findType _ SNil = Nothing
findType s (SCons (SPair id t) l) = findTypeHelper s id l

findTypeHelper :: SText s -> SText id -> SList l -> Maybe (VarWitness ('(id, t): l) s)
findTypeHelper s id l = case isEq s id of
    Just Refl -> Just Here
    Nothing -> There <$> findType s l

getType :: SList l -> VarWitness l s -> ProgType
getType (SCons (SPair _ t) _) Here = fromSingl t
getType (SCons (SPair _ _) ids) (There w) = getType ids w
getType SNil w = case w of

type family EVars (e :: Env Symbol) = r where
    EVars ('Env v) = v

type family GetType (s :: Symbol) (v :: Vars Symbol) (x :: VarWitness v s) where
    GetType s ('(s, st):t) Here = st
    GetType s ('(_, _):t) (There w) = GetType s t w

data Lit :: ProgType -> Type where
    LInt :: Int -> Lit PInt

data Expr :: ProgType -> Type where
    ELit :: Lit a -> Expr a
    EVar :: SEnvT e -> SText s -> SVarWitness w -> Expr (GetType s (EVars e) w)