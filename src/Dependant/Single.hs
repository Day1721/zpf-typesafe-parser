{-# LANGUAGE TypeInType, TypeFamilies, GADTs, RankNTypes, RebindableSyntax, FlexibleInstances, TypeOperators #-}

module Dependant.Single where

import Prelude hiding ((>>=))
import Data.Kind

-- data family Singl :: k -> Type

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

-- monadic operator, can use as (>>=) in 'do' block (see RebindableSyntax)
infixl 1 >=>
(>=>) :: WithSinglT
(>=>) = withSingl

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
    -- toSingl (h:t) = h >>= (\h' -> 
    --     t >>= (\t' -> 
    --     SomeSingl (SCons h' t'))) where
    --         (>>=) = (>=>)

-- TODO make this work :D
class Single a => ISingl (x :: a) where
    singl :: Singl x

-- TODO implement
instance ISingl (b::Bool) where
    singl = undefined

instance ISingl 'True where
    singl = STrue

instance ISingl 'False where
    singl = SFalse
