{-# LANGUAGE GADTs #-} 
{-# LANGUAGE TypeInType #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module TypeChecking.Basic where

import Data.Type.Equality
import GHC.TypeLits hiding (Text)
import Basic.Single
import Basic.TH
import Data.Kind



infixr :->
data ProgType str = PInt | PUnit | PStr | ProgType str :-> ProgType str | PData str
    deriving (Eq)

type VProgType = ProgType Text
type TProgType = ProgType Symbol

instance Show s => Show (ProgType s) where
    show PStr = "string"
    show PInt = "int"
    show PUnit = "()"
    show (PData name) = show name
    show (f :-> t) = show f ++ " -> " ++ show t


type family LiftProgType t where
    LiftProgType PInt = Int
    LiftProgType PUnit = ()
    LiftProgType PStr = String
    LiftProgType (x :-> y) = LiftProgType x -> LiftProgType y


infixr :~>
type instance Demote (ProgType str) = ProgType (Demote str)
instance Single s => Single (ProgType s) where
    data instance Singl :: Demote (ProgType s) -> Type where
        SPInt  :: SProgType PInt
        SPUnit :: SProgType PUnit
        SPStr  :: SProgType PStr
        SPData :: Singl s -> SProgType (PData s)
        (:~>)  :: SProgType a -> SProgType b -> SProgType (a :-> b)
    fromSingl SPInt = PInt
    fromSingl SPUnit = PUnit
    fromSingl SPStr = PStr
    fromSingl (SPData t) = PData (fromSingl t)
    fromSingl (x :~> y) = fromSingl x :-> fromSingl y

    toSingl PInt = SomeSingl SPInt
    toSingl PUnit = SomeSingl SPUnit
    toSingl PStr = SomeSingl SPStr
    toSingl (PData t) = 
        t >=> \t ->
        SomeSingl (SPData t)
    toSingl (x :-> y) = 
        x >=> \x ->
        y >=> \y ->
        SomeSingl (x :~> y)
type SProgType (x :: ProgType (Demote s)) = Singl x
type SProgTypeT (x :: ProgType (Demote Text)) = Singl x
instance EqDec s => EqDec (ProgType s) where
    SPInt  === SPInt  = Just Refl 
    SPUnit === SPUnit = Just Refl 
    SPStr  === SPStr  = Just Refl
    SPData tl === SPData tr =
        tl === tr >~>
        return Refl
    la :~> lr === ra :~> rr =
        la === ra >~>
        lr === rr >~>
        return Refl
    _ === _ = Nothing




-- environment type
-- s is essentially string, so 'Text' on term level and 'Symbol' on type level
type Vars s = [(s, ProgType s)]
type Datas s = [(s, [DataCon s])]
data Env s = Env (Vars s) (Datas s)

eVars  (Env v _) = v 
eDatas (Env _ d) = d

type instance Demote (Env s) = Env (Demote s)
instance Single s => Single (Env s) where
    data instance Singl (e :: Demote (Env s)) where
        SEnv :: SList (v :: [Demote (s, ProgType s)]) -> SList (d :: [Demote (s, [DataCon s])]) -> SEnv ('Env v d)
    fromSingl (SEnv v d) = Env (fromSingl v) (fromSingl d)
    toSingl (Env v d) = 
        v >=> \v ->
        d >=> \d ->
        SomeSingl $ SEnv v d
type SEnv (e :: Demote (Env s)) = Singl e
type SEnvT (e :: Demote (Env Text)) = SEnv e
instance EqDec s => EqDec (Env s) where
    SEnv lv ld === SEnv rv rd = 
        lv === rv >~>
        ld === rd >~>
        return Refl





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
    toSingl (There w) = 
        w >=> \w ->
        SomeSingl $ SThere w
type SVarWitness (w :: Demote (VarWitness l s)) = Singl w
instance EqDec (VarWitness l s) where
    SHere === SHere = Just Refl
    SThere lw === SThere rw = 
        lw === rw >~>
        return Refl
    _ === _ = Nothing

type family EVars (e :: Env Symbol) where
    EVars ('Env v _) = v
type family EDatas (e :: Env Symbol) where
    EDatas ('Env _ d) = d

type family GetType (s :: Symbol) (v :: Vars Symbol) (x :: VarWitness v s) where
    GetType s ('(s, st):t) Here = st
    GetType s (_:t) (There w) = GetType s t w


type GenFun (f :: Demote k1 -> Demote k2) = (Single k1, Single k2) => forall (a :: Demote k1). Singl a -> Singl (f a)

findType :: SText s -> SList l -> Maybe (VarWitness l s)
findType _ SNil = Nothing
findType s (SCons (SPair id t) l) = case s === id of
    Just Refl -> Just Here
    Nothing -> There <$> findType s l

getType :: SList l -> SVarWitness w -> SProgType (GetType s l w)
getType (SCons (SPair _ t) _) SHere = t
getType (SCons (SPair _ _) ids) (SThere w) = getType ids w
getType SNil w = case w of


data DataCon s = DataCon s [ProgType s] 

-- $(deriveDemote ''DataCon)
type instance Demote (DataCon s) = DataCon (Demote s)
instance (Single s) => Single (DataCon s) where
    data instance Singl (dc :: Demote (DataCon s)) where
        SDataCon :: Singl (str :: Demote s) -> SList l -> SDataCon ('DataCon str l)
    fromSingl (SDataCon str l) = DataCon (fromSingl str) (fromSingl l)
    toSingl (DataCon str l) = 
        str >=> \str ->
        l >=> \l ->
        SomeSingl $ SDataCon str l
type SDataCon (e :: Demote (DataCon s)) = Singl e
instance EqDec s => EqDec (DataCon s) where
    SDataCon ls ll === SDataCon rs rl =
        ls === rs >~>
        ll === rl >~>
        return Refl