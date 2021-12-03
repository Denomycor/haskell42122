
module Main where

import System.Environment (getArgs)
import Control.Monad (when)
import Cypher (cesar, subs, vigenere)
import EvalArgs (evalArgs)

main = do
    args <-getArgs
    if evalArgs args then
        loop args
    else
        putStrLn "Error - Input in wrong format!\nPlese try: Main {name of cypher} {enc or dec} {key for given cyper}"

loop args = do
    text <- getLine
    putStrLn $ cypher args text
    when (not $ null text) (loop args)

cypher :: [String] -> String -> String
cypher [c,d,k] t
    | c == "cesar" = cesar t (read k) (d=="enc")
    | c == "vigenere" = vigenere t k (d=="enc")
    | c == "substitui" = subs t k (d=="enc")
    | otherwise = "Error"
cypher _ _ = "Error"
