{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}

module Basic.Uninhabited (
    Uninhabited (..),
    Basic.Uninhabited.absurd,
    uninhabited
) where

import Data.Void as V
import Language.Haskell.TH as TH


class Uninhabited t where
    uninhabitedLift :: t -> Void

instance Uninhabited Void where
    uninhabitedLift = id


-- | Use an absurd assumption to discharge a proof obligation
-- | @ t some empty type
-- | @ a the goal type
-- | @ h the contradictory hypothesis
absurd :: Uninhabited t => t -> a
absurd h = V.absurd (uninhabitedLift h)


-- | Generates Uninhabited instance for given type
-- | Haskell warns you if type you passed isn't uninhabitable (then implement by hand or give up)
-- | @ tq empty type representation
uninhabited :: TH.TypeQ -> TH.DecsQ
uninhabited tq = tq >>= \t -> let
    rt = return (liftT t)
    liftT :: TH.Type -> TH.Type
    liftT = \case
        TH.ForallT _ _ t' -> liftT t'
        t' -> t'
    in [d| 
    instance Uninhabited $rt where
        uninhabitedLift x = case x of { } 
    |]

-- example of uninhabitable type: S n :~: Z where S - successor, Z - zero