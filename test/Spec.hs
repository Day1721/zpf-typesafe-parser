{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}

import Parser
import Text.Megaparsec
import Parser
import TypeChecking.TC
import Basic.TH
import Basic.Single
import System.Exit
import Basic.SingleInstances
import Str

data List a = Nila | Consa a (List a)
    deriving Show

$(deriveDemote ''Maybe)
$(deriveSingl ''Maybe)

$(deriveDemote ''List)
$(deriveSingl ''List)

test1 = SJust (SConsa STrue SNila)
test2 = fromSingl test1
test3 = toSingl test2



-- TODO add some tests
shouldPass :: [(String, String)]
shouldPass =
    ("just unit"    , "let tst = ()") :
    ("simple let"   , "let tst = let x = 42 in ()") :
    ("build-in use" , [str|let tst = print_endline "Hello World!"|]) :
    ("local id use" , "let tst = let x = () in x") :
    ("strange name" , "let tst = let _qazwsx_dx'98 = () in _qazwsx_dx'98") :
    ("lambda"       , "let tst = let x = fun (y: unit) -> y in x ()") :
    []

data Phase = PParser | PTypeChecker

shouldn'tPass :: [(String, Phase, String)]
shouldn'tPass =
    ("inval parameter", PTypeChecker, "let tst = print_endline ()") :
    ("undefined ident", PTypeChecker, [str|let tst = putStrLn "Hello World!"|]) :
    []

oneFail :: String -> Phase -> String -> IO ()
oneFail name PParser code = case runParsing name code of
    Left err -> return ()
    Right _ -> putStrLn ("Should fail in Parser, but haven't: " ++ name) >> exitWith (ExitFailure 4)
oneFail name PTypeChecker code = case runParsing name code of
    Left err -> putStrLn err >> exitWith (ExitFailure 2)
    Right ast -> case runChecker ast of
        Left err -> return ()
        Right _ -> putStrLn ("Should fail in TC, but haven't: " ++ name) >> exitWith (ExitFailure 5)

onePass :: String -> String -> IO ()
onePass name code = case runParsing name code of
    Left err -> putStrLn err >> exitWith (ExitFailure 2)
    Right ast -> case runChecker ast of
        Left err -> putStrLn err >> exitWith (ExitFailure 3)
        Right _ -> return ()

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z

main :: IO ()
main = do
    mapM_ (uncurry onePass) shouldPass
    mapM_ (uncurry3 oneFail) shouldn'tPass
    putStrLn "\nTH derivation test:"
    print test2
