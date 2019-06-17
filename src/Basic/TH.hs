{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{--# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Basic.TH where

import Basic.Single
import Language.Haskell.TH.Syntax
import Language.Haskell.TH

unimplemented :: a
unimplemented = error "not implemented yet"

deriveDemote :: Name -> Q [Dec]
deriveDemote ty = do
    (TyConI tyCon) <- reify ty
    (tyConName, tyVars, constructors) <- case tyCon of
        DataD _ name vars _ cs _ -> return (name, vars, cs)
        NewtypeD _ name vars _ c _ -> return (name, vars, [c])
        _ -> fail "deriveDemote: tyCon may not be a type synonym."
    let appliedDemote = foldl1 AppT appElements
        appElements = (ConT tyConName) : map ((ConT ''Demote `AppT`) . tvToT) tyVars
        tvToT = \x -> case x of
            PlainTV n -> VarT n
            KindedTV n _ -> VarT n
        appliedRaw = foldl1 appT (conT tyConName : map (return . tvToT) tyVars)
    demote <- tySynInstD ''Demote (tySynEqn [appliedRaw] (return appliedDemote))
    return [demote]
deriveSingl :: Name -> Q [Dec]
deriveSingl ty = do
    (TyConI tyCon) <- reify ty
    (tyConName, tyVars, constructors) <- case tyCon of
        DataD _ name vars _ cs _ -> return (name, vars, cs)
        NewtypeD _ name vars _ c _ -> return (name, vars, [c])
        _ -> fail "deriveSingl: tyCon may not be a type synonym."
    let instanceType = conT ''Single `appT` appliedType
        appliedType = foldl apply (conT tyConName) tyVars
        preds = sequence $ map (conT ''Single `apply`) tyVars
    
    sing <- genSingl appliedType tyConName tyVars constructors
    inst <- instanceD (preds) instanceType (map return sing)
    return [inst]
  where
    apply t (PlainTV name)    = appT t (varT name)
    apply t (KindedTV name _) = appT t (varT name)

genSingl :: TypeQ -> Name -> [TyVarBndr] -> [Con] -> Q [Dec]
genSingl tpq t vars cs = do
    appType <- tpq
    let kind = AppT (ConT ''Demote) appType
        vbtToBt (a,b,c) = (b,c)
        mkCon kvs n bts = do
            let nn = mkSname n
            (nbts, namedKinds) <- unzip <$> mapM convertArgument bts
            let names = map fst namedKinds
            return $ ForallC (kvs ++ map kindedProper namedKinds) []
                $ GadtC [nn] nbts (ConT ''Singl `AppT` (foldl AppT (PromotedT n) (map VarT names)))
        mkBangType t = (Bang NoSourceUnpackedness NoSourceStrictness, t)
        convertArgument (_,t) = do
            name <- newName "q"
            tp <- conT ''Singl `appT` (varT name)
            return (mkBangType tp, (name, (AppT (ConT ''Demote) t))) 
        kindedProper (n, t) = KindedTV n t
        mapper kvs = \c -> case c of
            RecC n bts -> mkCon kvs n (map vbtToBt bts)
            GadtC [n] bts t -> mkCon kvs n bts
            NormalC n bts -> mkCon kvs n bts
            _ -> undefined
        newCs = map (mapper vars) cs
    newCsu <- sequence newCs
    let pcs = zip cs newCsu
    sequence [
        dataInstD (return []) ''Singl [return $ SigT (VarT $ mkName "s") kind] Nothing newCs [],
        funD 'fromSingl (map genFromClause newCsu),
        funD 'toSingl (map genToClause cs)
     ]
    where
        getUnqualifiedName (Name (OccName s) _) = last $ split '.' s
        argToPat (_,t) = do
            a <- newName "a"
            return (varP a, a)
        stripSname = mkName . tail . getUnqualifiedName
        mkSname n = mkName ("S" ++ getUnqualifiedName n)
        genFromClause c = case c of
            ForallC ks _ (GadtC [name] args _) -> do
                (pats, names) <- unzip <$> mapM argToPat args
                p <- conP name pats
                return $ Clause [p] (NormalB (foldl AppE (ConE $ stripSname name) (map ((VarE 'fromSingl `AppE`) . VarE) names))) []
            _ -> undefined
        genToClause c = do
            (name, pats, names) <- case c of
                RecC n vbts -> do
                    (pats, names) <- unzip <$> mapM (\(a,b,c) -> argToPat (b,c)) vbts
                    return (n, pats, names)
                GadtC [n] bts _ -> do
                    (pats, names) <- unzip <$> mapM argToPat bts
                    return (n, pats, names)
                NormalC n bts -> do
                    (pats, names) <- unzip <$> mapM argToPat bts
                    return (n, pats, names)
                _ -> undefined
            p <- conP name pats
            return $ Clause [p] (NormalB 
                (CaseE 
                    (TupE (map ((VarE 'toSingl `AppE`) . VarE) names)) 
                    [Match (TupP (map (\n -> ConP 'SomeSingl [VarP n]) names)) (NormalB (ConE 'SomeSingl `AppE` foldl AppE (ConE $ mkSname name) (map VarE names))) []]
                    )) []

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s
