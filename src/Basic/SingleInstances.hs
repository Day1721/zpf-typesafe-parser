{-# LANGUAGE GADTs, TemplateHaskell, KindSignatures, TypeFamilies, DataKinds, TypeFamilyDependencies, FlexibleInstances, PolyKinds, QuasiQuotes #-}

module Basic.SingleInstances where

import Basic.Single
import Basic.TH
import Data.Type.Equality


$(deriveDemote ''Bool)
$(deriveSingl ''Bool)


type SBool b = Singl (b :: Bool)
instance EqDec Bool where
    STrue === STrue = return Refl
    SFalse === SFalse = return Refl
    _ === _ = Nothing

instance KnownSingle True where
    getSingl = STrue
instance KnownSingle False where
    getSingl = SFalse
