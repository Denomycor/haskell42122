
module EvalArgs(
    evalArgs
) where

import Text.Read (readMaybe)
import Data.Maybe (isJust)

evalArgs :: [String] -> Bool
evalArgs [c,d,k] = evalDirection d && evalCypherKey c k
evalArgs _ = False

evalDirection :: String -> Bool
evalDirection d = d == "enc" || d == "dec"

evalCypherKey :: String -> String -> Bool
evalCypherKey c k = (isJust (readMaybe k :: Maybe Int) && c=="cesar") || 
    (foldl (\acc x -> acc && x `elem` ['A'..'Z']) True k && (c=="vigenere" || c=="substitui"))