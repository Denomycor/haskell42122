{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import System.Environment ( getArgs )
import Cypher ( cesar, subs, vigenere )
import Control.Monad ( when )

main = do
    args <-getArgs
    text <- getLine
    putStrLn $ cypher args text
    when (not $ null text) main


cypher :: [String] -> String -> String
cypher [c,d,k] t
    | c == "cesar" = cesar t (read k) (d=="enc")
    | c == "vigenere" = vigenere t k (d=="enc")
    | c == "substitui" = subs t k (d=="enc")
    | otherwise = "Error"
