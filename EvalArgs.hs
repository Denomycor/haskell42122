{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module EvalArgs(
    evalArgs
) where

import Text.Read (readMaybe)
import Data.Maybe (isJust)

evalArgs :: [String] -> Bool
evalArgs a = (length a == 3) && evalArgsImp a

evalArgsImp :: [String] -> Bool
evalArgsImp [c,d,k] = evalDirection d && evalCypherKey c k

evalDirection :: String -> Bool
evalDirection d = d == "enc" || d == "dec"

evalCypherKey :: String -> String -> Bool
evalCypherKey c k = (isJust (readMaybe k :: Maybe Int) && c=="cesar") || 
    (foldl (\acc x -> acc && x `elem` ['A'..'Z']) True k && (c=="vigenere" || c=="substituir"))