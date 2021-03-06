module Main where

import System.Environment (getArgs)
import Control.Monad (when)
import Cypher(vigenere, substitute, cesar)
import EvalArgs (evalArgs)
import QuickTests (runAllTests)

main :: IO ()
main = do
    args <-getArgs
    if evalArgs args then
        loop args
    else if args == ["-t"] then
        runAllTests   
    else
        putStrLn "Error - Input in wrong format!\nPlese try: Main {name of cypher} {enc or dec} {key for given cyper} or Main -t"


loop :: [String] -> IO ()
loop args = do
    text <- getLine
    putStrLn $ cypher args text
    when (not $ null text) (loop args)



cypher :: [String] -> String -> String
cypher [c,d,k] t
    | c == "cesar" = cesar t (read k) (d=="enc")
    | c == "vigenere" = vigenere t k (d=="enc")
    | c == "substitui" = substitute t k (d=="enc")
    | otherwise = "Error"
cypher _ _ = "Error"

--Salvador Sobral, Amar pelos Dois
--LITUANIA
--8
--PAISEUROPA
