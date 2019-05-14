{-# LANGUAGE TypeInType, TypeFamilies, GADTs, RankNTypes, RebindableSyntax, FlexibleInstances, TypeOperators #-}

module Dependant.Single where

import Prelude hiding ((>>=))
import Data.Kind

-- existentional wrapper for Singl a
data SomeSingl k where
    SomeSingl :: Singl (a :: k) -> SomeSingl k

class Single k where
    data Singl :: k -> Type
    fromSingl :: Singl (a :: k) -> k
    toSingl :: k -> SomeSingl k

instance (Single k, Show k) => Show (Singl (a :: k)) where
    show = show . fromSingl

instance Eq (Singl a) where
    (==) _ _ = True

type WithSinglT = forall k r. Single k => k -> (forall (a :: k). Singl a -> r) -> r

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
instance Single Bool where
    data instance Singl (b :: Bool) where
        STrue :: Singl True
        SFalse :: Singl False
    fromSingl STrue = True
    fromSingl SFalse = False
    
    toSingl True = SomeSingl STrue
    toSingl False = SomeSingl SFalse
type SBool b = Singl (b :: Bool)

instance Single a => Single [a] where
    data instance Singl (l :: [a]) where
        SNil :: Singl '[]
        SCons :: Singl x -> Singl l -> Singl (x:l)
    fromSingl SNil = []
    fromSingl (SCons x l) = (fromSingl x) : (fromSingl l)

    toSingl [] = SomeSingl SNil
    toSingl (h:t) = do
        h <- h
        t <- t
        SomeSingl (SCons h t) where
            (>>=) :: WithSinglT
            (>>=) = (>=>)
