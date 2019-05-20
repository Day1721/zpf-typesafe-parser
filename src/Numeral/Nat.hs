{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Numeral.Nat where

import Basic.Single
import Basic.Uninhabited

import Prelude
import Data.Type.Equality

--TODO move to binary nat or 'KnownNat' for performance reasons
data Nat = Zero | Succ Nat
    deriving (Eq)

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

intToNat :: Int -> Nat
intToNat x = if x > 0 then Succ $ intToNat $ x-1 else Zero


instance Show Nat where
    show = show . natToInt


type family (n :: Nat) + (m :: Nat) where
    Zero + m = m
    (Succ n) + m = Succ (n + m)

type family (n :: Nat) - (m :: Nat) where
    m - Zero = m
    Zero - n = Zero
    Succ n - Succ m = n - m

type family Min (n :: Nat) (m :: Nat) where
    Min Zero m = Zero
    Min n Zero = Zero
    Min (Succ n) (Succ m) = Succ (Min n m)

-- Vals (Finite n) ~ [0, n]
data Finite (n :: Nat) where
    FZ :: Finite (Succ n)
    FS :: Finite n -> Finite (Succ n)

$(uninhabited [t| Finite Zero |])

-- data SNat' (n :: Nat) where
--     SZero' :: SNat' Zero
--     SSucc' :: SNat' n -> SNat' (Succ n)

type instance Demote Nat = Nat
instance Single Nat where
    data instance Singl (n :: Nat) where
        SZero :: SNat Zero
        SSucc :: SNat n -> SNat (Succ n)
    fromSingl SZero = Zero
    fromSingl (SSucc n) = Succ (fromSingl n)
    
    toSingl Zero = SomeSingl SZero
    toSingl (Succ n) = n >=> \n' -> SomeSingl (SSucc n')

type SNat n = Singl (n :: Nat)

(%+) :: SNat n -> SNat m -> SNat (n + m)
SZero   %+ m = m
SSucc n %+ m = SSucc (n %+ m)


data Parity (a :: Nat) where
    Even :: SNat n -> Parity (n + n)
    Odd :: SNat n -> Parity (Succ (n + n))

parity :: SNat m -> Parity m
parity SZero = Even SZero
parity (SSucc SZero) = Odd SZero
parity (SSucc (SSucc n)) = case (parity n) of
    Even j -> case plusSuccDist j j of
        Refl -> Even (SSucc j)
    Odd j -> case plusSuccDist j j of
        Refl -> Odd (SSucc j)

-- REGION PROOFS

zerozero :: SNat n -> SNat m -> (n + m :~: Zero) -> (n :~: Zero, m :~: Zero)
zerozero SZero SZero = const (Refl, Refl)
zerozero (SSucc _) _ = absurd
zerozero n (SSucc s) = absurd . trans (sym $ plusSuccDist n s)

plusZero :: SNat n -> (n + Zero) :~: n
plusZero SZero = Refl
plusZero (SSucc n) = apply Refl (plusZero n)

plusSuccDist :: SNat n -> SNat m -> n + (Succ m) :~: Succ (n + m)
plusSuccDist SZero _ = Refl
plusSuccDist (SSucc n) m = apply Refl (plusSuccDist n m)

plusComm :: SNat n -> SNat m -> n + m :~: m + n
plusComm n SZero = plusZero n
plusComm n (SSucc m) = trans (plusSuccDist n m) (apply Refl $ plusComm n m)

plusAssos :: SNat n -> SNat m -> SNat r -> (n + m) + r :~: n + (m + r)
plusAssos SZero _ _ = Refl
plusAssos (SSucc n) m r = apply Refl $ plusAssos n m r 

-- ENDREGION PROOFS

$(uninhabited [t| forall n. Succ n :~: Zero |])
