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
        _ -> fail "genSingl: tyCon may not be a type synonym."
    demote <- tySynInstD ''Demote (tySynEqn [conT tyConName] (conT tyConName))
    return [demote]
deriveSingl :: Name -> Q [Dec]
deriveSingl ty = do
    (TyConI tyCon) <- reify ty
    (tyConName, tyVars, constructors) <- case tyCon of
        DataD _ name vars _ cs _ -> return (name, vars, cs)
        NewtypeD _ name vars _ c _ -> return (name, vars, [c])
        _ -> fail "genSingl: tyCon may not be a type synonym."
    let instanceType = conT ''Single `appT`
            (foldl apply (conT tyConName) tyVars)
    
    sing <- genSingl tyConName tyVars constructors
    inst <- instanceD (return []) instanceType (map return sing)
    return [inst]
  where
    apply t (PlainTV name)    = appT t (varT name)
    apply t (KindedTV name _) = appT t (varT name)

genSingl :: Name -> [TyVarBndr] -> [Con] -> Q [Dec]
genSingl t vars cs = do
    let kind = AppT (ConT ''Demote) (ConT t)
        vbtToBt (a,b,c) = (b,c)
        getUnqualifiedName (Name (OccName s) _) = last $ split '.' s
        mkCon n bts =
            let nn = mkName ("S" ++ getUnqualifiedName n) in
            GadtC [nn] {-TODO-}bts (ConT ''Singl `AppT` PromotedT n)
        mapper = \c -> case c of
            RecC n bts -> mkCon n (map vbtToBt bts)
            GadtC [n] bts t -> mkCon n bts
            NormalC n bts -> mkCon n bts
            _ -> undefined
        newCs = map (return . mapper) cs
    sequence [
        dataInstD (return []) ''Singl [return $ SigT (VarT $ mkName "a") kind] Nothing newCs [],
        funD 'fromSingl [genFromClause ()],--(map genFromClause cs),
        funD 'toSingl [genFromClause ()]--(map genToClause cs)
     ]
    where
        genFromClause c = return $ Clause [WildP] (NormalB (VarE (mkName "undefined"))) []
        genToClause c = return $ Clause [WildP] (NormalB (VarE (mkName "undefined"))) []
-- genFmapClause :: Con -> Q Clause
-- genFmapClause c@(NormalC name fieldTypes)
--    = do f          <- newName "f"
--         fieldNames <- replicateM (length fieldTypes) (newName "x")

--         let pats = varP f:[conP name (map varP fieldNames)]
--             body = normalB $ appsE $
--                 conE name : map (newField f) (zip fieldNames fieldTypes)

--         clause pats body []
--     where
--         (>>=) = Prelude.(>>=)

-- newField :: Name -> (Name, StrictType) -> Q Exp
-- newField f (x, (_, fieldType))
--     = do 
--         Just (Deriving typeCon typeVar) <- getQ
--         case fieldType of
--             VarT typeVar' | typeVar' == typeVar ->
--                 [| $(varE f) $(varE x) |]
--             ty `AppT` VarT typeVar' |
--                 leftmost ty == (ConT typeCon) && typeVar' == typeVar ->
--                 [| fmap $(varE f) $(varE x) |]
--             _ -> [| $(varE x) |]
--     where
--         (>>=) = Prelude.(>>=)

-- leftmost :: Type -> Type
-- leftmost (AppT ty1 _) = leftmost ty1
-- leftmost ty           = ty

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s
