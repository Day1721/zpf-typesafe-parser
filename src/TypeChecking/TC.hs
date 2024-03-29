{-# LANGUAGE GADTs #-} 
{-# LANGUAGE TypeInType #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImpredicativeTypes #-}

module TypeChecking.TC where

import Data.Kind
import Data.Type.Equality
import Control.Monad.Except hiding ((>=>))
import GHC.TypeLits hiding (Text)
import Basic.Single
import Basic.TH
import TypeChecking.Basic
import TypeChecking.Ast
import TypeChecking.StdLib
import qualified SyntaxTree as ST

toProgType :: ST.BType a -> VProgType
toProgType (ST.TInt _)     = PInt
toProgType (ST.TUnit _)    = PUnit
toProgType (ST.TStr _)     = PStr
toProgType (ST.TData s _)  = PData (Text s)
toProgType (ST.TFun x y _) = toProgType x :-> toProgType y

checker :: ST.Expr ST.Position -> Either String (Expr PUnit)
checker expr = 
    Env stdLib [] >=> \e -> 
    runExcept (checkExpr e expr) >>= \case
        SomeExpr SPUnit e -> Right e
        SomeExpr t e -> Left $ "Invalid type: expected unit, but got " ++ show t

type family UpdateEnv s t e where
    UpdateEnv s t ('Env v d) = 'Env ('(s,t):v) d

updateEnv :: SText s -> SProgType t -> SEnvT e -> SEnvT (UpdateEnv s t e)
updateEnv s t (SEnv v d) = SEnv (SCons (SPair s t) v) d

runChecker :: ST.Program ST.Position -> Either String [TopDef Text]
runChecker (ST.Program l) = Env stdLib [] >=> \e -> 
    runExcept $ checkTopDefs e l

checkTopDefs :: SEnvT e -> [ST.TopDef ST.Position] -> Except String [TopDef Text]
checkTopDefs _ [] = return []
checkTopDefs e (h:t) = checkTopDef e h >>= \(td, SomeSingl e') ->
    checkTopDefs e' t >>= \tds ->
    return (td:tds)

checkTopDef :: SEnvT e -> ST.TopDef ST.Position -> Except String (TopDef Text, SomeSingl (Env Text))
checkTopDef e (ST.DefLet ld@(ST.DLet ident _ _) _) = 
    checkDecl e ld >>= \(e', SomeExpr eT eTE) -> 
    return (DefLet $ LetDecl (Text ident) eTE, e')
checkTopDef e (ST.DefData ident cons _) = unimplemented

checkDecl :: SEnvT e -> ST.LetDeclaration ST.Position -> Except String (SomeSingl (Env Text), SomeExpr)
checkDecl env (ST.DLet id expr _) = 
    checkExpr env expr >>= \(SomeExpr exprT exprTE) ->
    Text id >=> \id ->
    return (SomeSingl (updateEnv id exprT env), SomeExpr exprT exprTE)

checkExpr :: SEnvT e -> ST.Expr ST.Position -> Except String SomeExpr
checkExpr e = \case
    ST.ELit l _ -> case checkLit l of
        SomeLit t e -> return $ SomeExpr t $ ELit e
        
    ST.EVar id p -> 
        Text id >=> \id -> 
        case e of 
            SEnv v _ -> case findType id v of
                Just w -> w >=> \w -> 
                    return $ SomeExpr (getType v w) $ EVar e id w
                Nothing -> throwError $ "Undefined variable" ++ show id ++ "at: " ++ show p

    ST.ELet decl@(ST.DLet ident _ _) expr _ -> 
        checkDecl e decl >>= \case
            (SomeSingl e', SomeExpr leT le) -> 
                checkExpr e' expr >>= \(SomeExpr e'T e'TE) ->
                return $ SomeExpr e'T $ ELet (LetDecl (Text ident) le) e'TE 

    ST.EApp fun param _ -> 
        checkExpr e fun >>= \(SomeExpr funT funTE) ->
        checkExpr e param >>= \(SomeExpr paramT paramTE) ->
        case funT of 
            arg :~> res -> case paramT === arg of
                Just Refl -> return $ SomeExpr res (EApp funTE paramTE)
                Nothing -> throwError $ "Incompatible parameter type\nexpected: " ++ show arg ++ "\ngot:      " ++ show paramT
            _ -> throwError $ "Non-fuctional type in application: " ++ show funT

    ST.ELambda x t expr _ -> 
        Text x >=> \x -> 
        toProgType t >=> \t -> 
        let e' = updateEnv x t e in
        checkExpr e' expr >>= \(SomeExpr exprT exprTE) ->
        return $ SomeExpr (t :~> exprT) $ EAbs x t exprTE

    ST.EMatch exp alts _ ->
        checkExpr e exp >>= \(SomeExpr te exp) ->
        mapM (checkAlt e te) alts >>= \salts@(SomeAlt tp u a : _) ->
        case te === tp of
            Nothing -> fail "Something went wrong"
            Just Refl ->
                let l = getAlts tp u salts in
                return $ SomeExpr u (EMatch exp l)

  where
    getAlts :: SProgTypeT t -> SProgType u -> [SomeAlt] -> [Alt t u]
    getAlts t u [] = []
    getAlts t u (SomeAlt p e h:ts) = 
        case t === p of
            Nothing -> undefined
            Just Refl -> case u === e of
                            Nothing -> undefined
                            Just Refl -> h : getAlts p e ts
    
checkAlt :: SEnvT e -> SProgTypeT t -> ST.Alt ST.Position -> Except String SomeAlt
checkAlt e t (ST.Alt p exp _) =
    checkExpr e exp >>= \(SomeExpr t2 exp) ->
    checkPattern e t p >>= \(SomePat t3 p) ->
    case t === t3 of
        Nothing -> fail "Incompatible type of pattern" --TODO position
        Just Refl -> return $ SomeAlt t t2 (Alt p exp)

checkPattern :: SEnvT e -> SProgTypeT t -> ST.Pattern ST.Position -> Except String SomePat
checkPattern e tsuggestion (ST.PVar name _) = return (SomePat tsuggestion (PVar tsuggestion (Text name)))
checkPattern e _ (ST.PLit l _) = case checkLit l of
                                    SomeLit t l -> return (SomePat t (PLit l))
checkPattern e _ (ST.PCon name ps _) = undefined

checkLit :: ST.Literal a -> SomeLit
checkLit = \case
    ST.LUnit     _ -> SomeLit SPUnit  LUnit
    ST.LInt    i _ -> SomeLit SPInt  (LInt i)
    ST.LString s _ -> SomeLit SPStr  (LStr s)
    _ -> unimplemented
