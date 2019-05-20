{-# LANGUAGE GADTs #-} 
{-# LANGUAGE TypeInType #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}

module Typechecker where

import Basic.Single
import GHC.TypeLits

data ProgType = PInt | PFun ProgType ProgType

type family LiftProgType t where
    LiftProgType PInt = Int
    LiftProgType (PFun x y) = LiftProgType x -> LiftProgType y

-- environment type
-- s is essentially string, so 'Text' on term level and 'Symbol' on type level
data Env s = Env [(s, ProgType)]

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
type SEnv (x :: Demote (Env s)) = Singl x
