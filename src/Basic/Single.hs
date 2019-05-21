{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Basic.Single where

import Prelude hiding ((>>=))
import GHC.TypeLits hiding (Text)
import Data.Kind
import Data.Proxy
import Data.String
import Data.Type.Equality
import Data.Reflection (reifySymbol)
import Unsafe.Coerce

-- wrapper for string just to distinguish this from [a] in Demote (String = [Char])
newtype Text = Text { tStr :: String }
    deriving (Eq, Show, Read, IsString)

-- | specifies a kind of type-level value of given type, bijection
-- | ex. Demote Bool = Bool
-- | but Demote Text = Symbol (see Symbol in GHC.TypeLits, essentially we can't have string of kind 'String', need to use 'Symbol')
-- | @ k one of types of (term-type) conversion
type family Demote k = r | r -> k

-- | existentional wrapper for Singl a
data SomeSingl k where
    SomeSingl :: Singl (a :: Demote k) -> SomeSingl k

class Single k where
    data Singl :: Demote k -> Type
    fromSingl :: Singl (a :: Demote k) -> k
    toSingl :: k -> SomeSingl k

instance (Single k, Show k) => Show (Singl (a :: Demote k)) where
    show = show . fromSingl

instance Eq (Singl a) where
    (==) _ _ = True

type WithSinglT = forall k r. Single k => k -> (forall (a :: Demote k). Singl a -> r) -> r

withSingl :: WithSinglT
withSingl x f = case toSingl x of
    SomeSingl y -> f y

-- monadic operator, can use as (>>=) in 'do' block (see RebindableSyntax, return |=> SomeSingl)
infixl 1 >=>
(>=>) :: WithSinglT
(>=>) = withSingl

-- when using a RebindableSyntax for 'do' block, you can just define a top-level (>>=) operator
-- to behave like (>=>) with the same type, so you wouldn't write 'where' part every time
-- (then if you want to use 'do' with the monad, just add "where (>>=) = Prelude.(>>=)")

-- TODO generation
type instance Demote Bool = Bool
instance Single Bool where
    data instance Singl (b :: Bool) where
        STrue :: SBool True
        SFalse :: SBool False
    fromSingl STrue = True
    fromSingl SFalse = False
    
    toSingl True = SomeSingl STrue
    toSingl False = SomeSingl SFalse
type SBool b = Singl (b :: Bool)

type instance Demote [a] = [Demote a]
instance Single a => Single [a] where
    data instance Singl (l :: Demote [a]) where
        SNil :: SList '[]
        SCons :: Singl x -> SList l -> SList (x:l)
    fromSingl SNil = []
    fromSingl (SCons x l) = (fromSingl x) : (fromSingl l)

    toSingl [] = SomeSingl SNil
    toSingl (h:t) = do
        h' <- h
        t' <- t
        SomeSingl (SCons h' t') where
            (>>=) :: WithSinglT
            (>>=) = (>=>)
type SList (l :: Demote [a]) = Singl l

type instance Demote (a,b) = (Demote a, Demote b)
instance (Single a, Single b) => Single (a,b) where
    data instance Singl (p :: Demote (a, b)) where
        SPair :: Singl (l :: Demote a) -> Singl (r :: Demote b) -> Singl '(l,r)
    fromSingl (SPair l r) = (fromSingl l, fromSingl r)
    toSingl (l, r) = l >=> \l' ->
        r >=> \r' ->
        SomeSingl (SPair l' r')
type SPair (x :: Demote (a,b)) = Singl x

type instance Demote Text = Symbol
type instance Demote Symbol = Text
instance Single Text where
    data instance Singl :: Symbol -> Type where
        SText :: KnownSymbol s => Proxy s -> Singl s
    fromSingl (SText p) = Text $ symbolVal p
    toSingl t = reifySymbol (tStr t) (\p -> SomeSingl $ SText p)
type SText (x :: Symbol) = Singl x

-- TODO find better way to get simular effect
isEq :: SText l -> SText r -> Maybe (l :~: r)
isEq l r = case fromSingl l == fromSingl r of
    True -> Just (unsafeCoerce Refl)
    False -> Nothing