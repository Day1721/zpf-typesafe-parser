{-# LANGUAGE OverloadedStrings #-}

module TypeChecking.StdLib where

import Basic.Single
import TypeChecking.Basic
import TypeChecking.Ast

-- to use as start env in TC
stdLib :: [(Text, VProgType)]
stdLib = 
    ("print_string",  PStr :-> PUnit) :
    ("print_endline", PStr :-> PUnit) :
    []