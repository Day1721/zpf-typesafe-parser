{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Basic.Single where

import Prelude
import GHC.TypeLits hiding (Text)
import Data.Kind
import Data.Proxy
import Data.String
import Data.Type.Equality
import Data.Reflection (reifySymbol)


-- wrapper for string just to distinguish this from [a] in Demote (String = [Char])
newtype Text = Text { tStr :: String }
    deriving (Eq, IsString)

instance Show Text where
    show (Text t) = t
instance Read Text where
    readsPrec _ str = [(Text str, "")]

-- | specifies a kind of type-level value of given type, bijection
-- | ex. Demote Bool = Bool
-- | but Demote Text = Symbol (see Symbol in GHC.TypeLits, essentially we can't have string of kind 'String', need to use 'Symbol')
-- | @ k one of types of (term-type) conversion
type family Demote k = r | r -> k

-- | existentional wrapper for Singl a
data SomeSingl k where
    SomeSingl :: Singl (a :: Demote k) -> SomeSingl k

instance EqDec k => Eq (SomeSingl k) where
    (SomeSingl l) == (SomeSingl r) = case l === r of
        Just Refl -> True
        Nothing -> False


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

-- consider return type in form "l = r \/ ~ l = r"
infix 4 ===
class Single k => EqDec k where
    (===) :: Singl (l :: Demote k) -> Singl (r :: Demote k) -> Maybe (l :~: r)

instance EqDec k => TestEquality (Singl :: Demote k -> Type) where
    testEquality l r = l === r

class Single a => KnownSingle (x :: Demote a) where
    getSingl :: Singl x


-- when using a RebindableSyntax for 'do' block, you can just define a top-level (>>=) operator
-- to behave like (>=>) with the same type, so you wouldn't write 'where' part every time
-- (then if you want to use 'do' with the monad, just add "where (>>=) = Prelude.(>>=)")

-- TODO generation

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
        return (SCons h' t') where
            (>>=) :: WithSinglT
            (>>=) = (>=>)
            return = SomeSingl
type SList (l :: Demote [a]) = Singl l
instance EqDec a => EqDec [a] where
    SNil === SNil = Just Refl
    SCons lh lt === SCons rh rt =
        lh === rh >~>
        lt === rt >~>
        return Refl
    _ === _ = Nothing

type instance Demote (a,b) = (Demote a, Demote b)
instance (Single a, Single b) => Single (a,b) where
    data instance Singl (p :: Demote (a, b)) where
        SPair :: Singl (l :: Demote a) -> Singl (r :: Demote b) -> Singl '(l,r)
    fromSingl (SPair l r) = (fromSingl l, fromSingl r)
    toSingl (l, r) = l >=> \l' ->
        r >=> \r' ->
        SomeSingl (SPair l' r')
type SPair (x :: Demote (a,b)) = Singl x
instance (EqDec a, EqDec b) => EqDec (a,b) where
    SPair la lb === SPair ra rb =
        la === ra >~>
        lb === rb >~>
        return Refl

type instance Demote Text = Symbol
type instance Demote Symbol = Text
instance Single Text where
    data instance Singl :: Symbol -> Type where
        SText :: KnownSymbol s => Proxy s -> Singl s
    fromSingl (SText p) = Text $ symbolVal p
    toSingl t = reifySymbol (tStr t) (\p -> SomeSingl $ SText p)
type SText (x :: Symbol) = Singl x
instance EqDec Text where
    (SText l) === (SText r) = sameSymbol l r

instance KnownSymbol s => KnownSingle s where
    getSingl = mkText @s


type instance Demote (Proxy a) = Proxy (Demote a)
instance Single a => Single (Proxy a) where
    data instance Singl :: Proxy (Demote a) -> Type where
        SProxy :: Singl (p :: Proxy (Demote a))
    fromSingl :: forall p. Singl (p :: Proxy (Demote a)) -> Proxy a
    fromSingl (SProxy :: Singl (p :: Proxy (Demote a))) = (Proxy :: Proxy a)
    toSingl (Proxy :: Proxy a) = SomeSingl (SProxy :: (Singl ('Proxy :: Proxy (Demote a))))


-- usage:
-- mktext @"Hello world!"
mkText :: forall s. KnownSymbol s => SText s
mkText = SText (Proxy @s)


cong :: forall f a b.a :~: b -> f a :~: f b
cong Refl = Refl

withEq :: (a :~: b) -> (a ~ b => r) ->r
withEq Refl x = x

withEqM :: Monad m => m (a :~: b) -> (a ~ b => m r) -> m r
withEqM m x = m >>= \Refl -> x

infixr 1 >~>
(>~>) :: Monad m => m (a :~: b) -> (a ~ b => m r) -> m r
(>~>) = withEqM
