{-# LANGUAGE GADTs, DeriveFunctor #-}
module SyntaxTree where

class Contextual t where
  context :: t a -> a

type Program a = [TopDef a]

data TopDef a =
      DefLet  (LetDeclaration a)  a
    | DefData String [DataCon a] a
  deriving (Eq, Show, Functor)

data DataCon a = DataCon String [BType a] a
  deriving (Eq, Show, Functor)

data Literal a =
      LInt    Int     a
    | LDouble Double  a
    | LChar   Char    a
    | LString String  a
    | LUnit           a
    | LBool   Bool    a
  deriving (Eq, Show, Functor)

data BType a =
      TInt  a
    | TUnit a
    | TStr  a
    | TData String a
    | TFun  (BType a) (BType a)  a 
    | TData String    a
  deriving (Eq, Show, Functor)

data Expr a =
      ELit    (Literal a)  a
    | EVar    String  a
    | ELet    (LetDeclaration a) (Expr a)  a
    | EApp    (Expr a) (Expr a)  a
    | ELambda String (BType a) (Expr a)  a
  deriving (Eq, Show, Functor)

data LetDeclaration a =
      DLet    String (Expr a) a
  deriving (Eq, Show, Functor)

instance Contextual TopDef where
  context (DefLet _ c) = c
  context (DefData _ _ c) = c

instance Contextual DataCon where
  context (DataCon _ _ c) = c

instance Contextual Literal where
  context (LInt _ c) = c
  context (LDouble _ c) = c
  context (LChar _ c) = c
  context (LString _ c) = c
  context (LUnit c) = c
  context (LBool _ c) = c

instance Contextual BType where
  context (TInt c) = c
  context (TUnit c) = c
  context (TStr c) = c
  context (TData _ c) = c
  context (TFun _ _ c) = c
  context (TData _ c) = c

instance Contextual Expr where
  context (ELit _ c) = c
  context (EVar _ c) = c
  context (ELet _ _ c) = c
  context (EApp _ _ c) = c
  context (ELambda _ _ _ c) = c

instance Contextual LetDeclaration where
  context (DLet _ _ c) = c
