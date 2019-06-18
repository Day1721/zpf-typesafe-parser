{-# LANGUAGE GADTs,
             DataKinds, 
             TypeOperators, 
             TypeFamilies, 
             FlexibleInstances, 
             FlexibleContexts,
             RankNTypes,
             AllowAmbiguousTypes,
             UndecidableInstances,
             PolyKinds
  #-}

{- The module defines a well-typed list whic  can contain values of different types (aka.(aka. Heterogeneous lists)
   At the beginning I planed to try HList library for it, but because after few tries it haven't installed I decided to try to write one by myself
   (I throught that it would be a great experience)
   Made in a way to be simular to standard Haskell list
   TODO add fusion with hbuild for increased performace
-}
module HList where

import Numeral.Nat
import Data.Kind

infixr 5 :>:
data HList (l :: [Type]) where
    HNil :: HList '[]
    (:>:) :: h -> HList t -> HList (h:t)

-- TODO why not works??
-- instance (All Show l) => Show (HList l) where
--     show HNil = "[]"
--     show (HCons h t) = "[" ++ hfoldl (\a v -> a ++ ", " ++ show v) (show h) t ++ "]" 

-- class All (f :: * -> Constraint) (l :: [*]) where
-- instance All f '[] where
-- instance (f h, All f t) => All f (h:t) where

instance Show (HList '[]) where
    show HNil = "H[]"
instance {-# Overlaps #-} Show h => Show (HList '[h]) where
    show (v :>: HNil) = "H[" ++ show v ++ "]"
instance (Show h, Show (HList t)) => Show (HList (h:t)) where
    show (h :>: t) = "H[" ++ show h ++ ", " ++ drop 2 (show t)


instance Eq (HList '[]) where
    HNil == HNil = True
instance (Eq h, Eq (HList t)) => Eq (HList (h:t)) where
    (lh :>: lt) == (rh :>: rt) = lh == rh && lt == rt

instance Ord (HList '[]) where
    HNil <= HNil = True
instance (Ord h, Ord (HList t)) => Ord (HList (h:t)) where
    (lh :>: lt) <= (rh :>: rt) = lh <= rh || (lh > rh && lt <= rt)

-- type SingleHListType a = (a : '[])

--instance Functor (Compose HList SingleHListType) where

hnull :: HList l -> Bool -- HList '[] -> Bool ??
hnull HNil = True
hnull _ = False

hhead :: HList (h:t) -> h
hhead (h :>: t) = h 

htail :: HList (h:t) -> HList t
htail (h :>: t) = t

hlength :: HList l -> Int
hlength = hfoldl (\a _ -> a+1) 0

hslength :: HList l -> SNat (Length l)
hslength HNil = SZero
hslength (h :>: t) = SSucc (hslength t)

infixr 5 <++>
(<++>) :: HList l1 -> HList l2 -> HList (l1 ++ l2)
(<++>) HNil l2 = l2 
(<++>) (h :>: t) l2 = h :>: t <++> l2

hreverse :: HList l -> HList (Reverse l)
hreverse HNil = HNil
hreverse (h :>: t) = (hreverse t) <++> (h :>: HNil)

hfoldl :: (forall a. b -> a -> b) -> b -> HList l -> b
hfoldl _ acc HNil = acc
hfoldl f acc (h :>: t) = hfoldl f (f acc h) t

hmap :: forall b l. (forall a. a -> b a) -> HList l -> HList (Map b l)
hmap _ HNil = HNil
hmap f (h :>: t) = f h :>: hmap f t

hbuild :: forall r. (forall l. (forall h t. h -> l t -> l (h:t)) -> l '[] -> l r) -> HList r
hbuild f = f (:>:) HNil

type family l1 ++ l2 where
    '[] ++ l2 = l2
    (h:t) ++ l2 = h : t ++ l2

type family Map f l where
    Map f '[] = '[]
    Map f (h:t) = (f h : Map f t)

apply :: Func a b -> HList a -> b
apply f HNil = f
apply f (h :>: t) = apply (f h) t

type family Func args res where
    Func '[] res = res
    Func (h:t) res = h -> Func t res

type family Length l where
    Length '[] = Zero
    Length (h:t) = Succ (Length t)

type family Compose f g t where
    Compose f g t = f (g t)

type family Reverse l where
    Reverse '[] = '[]
    Reverse (h:t) = (Reverse t) ++ '[h]

type family l !! n where
    (h:t) !! Zero = h
    (h:t:ts) !! (Succ n) = (t:ts) !! n

type family Take n l where
    Take Zero _ = '[]
    Take (Succ n) (h:t) = h : Take n t

type family Skip n l where
    Skip Zero l = l
    Skip (Succ n) (_:t) = Skip n t

--problems:
--  Type Composition (because type family cannot be used without all parameters filled)
--  type alias cannot be used without all parameters filled :(

-- data Struct (t :: [(String, Type)]) where

-- type family HList (elems :: [Type]) = r | r -> elems where
--     HList '[] = ()
--     HList (h:t) = (h, HList t)

-- infixr 5 <:>
-- (<:>) :: h -> HList t -> HList (h:t)
-- (<:>) v l = (v,l)

-- -- []
-- hempty :: HList '[]
-- hempty = ()

-- hhead :: HList (h:t) -> h
-- hhead (v,_) = v

-- htail :: HList (h:t) -> HList t
-- htail (_,t) = t

-- hlength :: HList l -> Int
-- hlength (() :: HList l) = 0
-- hlength (h, t) = 1 + hlength t

-- build :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
-- would like to have more polymorphic type:
-- hbuild :: forall r. (forall l. (forall h t. h -> l t -> l (h:t)) -> l '[] -> l r) -> HList r
-- hbuild :: forall r. ((forall h t. h -> HList t -> HList (h:t)) -> HList '[] -> HList r) -> HList r
-- hbuild f = f (<:>) hempty

data SHList (t :: HList l) where
    SHNil :: SHList HNil
    SHCons :: x -> SHList xs -> SHList (x :>: xs)
