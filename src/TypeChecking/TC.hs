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
import qualified SyntaxTree as ST

toProgType :: ST.BType a -> ProgType
toProgType (ST.TInt _)     = PInt
toProgType (ST.TUnit _)    = PUnit
toProgType (ST.TFun x y _) = PFun (toProgType x) (toProgType y)


checker :: ST.Expr a -> Either String (Expr PUnit)
checker expr = let e = SEnv SNil in runExcept (runChecker e expr) >>= \case
    SomeExpr SPUnit e -> Right e
    SomeExpr t e -> Left $ "Invalid type: expected unit, but got " ++ show t

type family UpdateEnv s t e where
    UpdateEnv s t ('Env v) = 'Env ('(s,t):v)

updateEnv :: SText s -> SProgType t -> SEnvT e -> SEnvT (UpdateEnv s t e)
updateEnv s t (SEnv v) = SEnv $ SCons (SPair s t) v

checkDecl :: ST.LetDeclaration a -> SEnvT e -> Except String (SomeSingl (Env Text))
checkDecl (ST.DLet id expr _) env = 
    runChecker env expr >>= \(SomeExpr exprT exprTE) ->
    Text id >=> \id ->
    return $ SomeSingl $ updateEnv id exprT env

runChecker :: SEnvT e -> ST.Expr a -> Except String SomeExpr
runChecker e = \case
    ST.ELit l _ -> case checkLit l of
        SomeLit t e -> return $ SomeExpr t $ ELit e
        
    ST.EVar id _ -> 
        Text id >=> \id -> 
        case e of 
            SEnv v -> case findType id v of
                Just w -> w >=> \w -> 
                    return $ SomeExpr (getType v w) $ EVar e id w
                Nothing -> throwError $ "Undefined variable" ++ show id

    ST.ELet decl expr _ -> checkDecl decl e >>= \case
        SomeSingl e' -> runChecker e' expr

    ST.EApp fun param _ -> runChecker e fun >>= \(SomeExpr funT funTE) ->
        runChecker e param >>= \(SomeExpr paramT paramTE) ->
        case funT of 
            SPFun arg res -> case paramT === arg of
                Just Refl -> return $ SomeExpr res (EApp funTE paramTE)
                Nothing -> throwError $ "Incompatible parameter type\nexpected: " ++ show arg ++ "\ngot:" ++ show paramT
            _ -> throwError $ "Non-fuctional type in application: " ++ show funT

    ST.ELambda x t expr _ -> 
        Text x >=> \x -> 
        toProgType t >=> \t -> 
        let e' = updateEnv x t e in
        runChecker e' expr >>= \(SomeExpr exprT exprTE) ->
        return $ SomeExpr (SPFun t exprT) $ EAbs x t exprTE

checkLit :: ST.Literal a -> SomeLit
checkLit = \case
    ST.LInt i _ -> SomeLit SPInt (LInt i)
    _ -> unimplemented