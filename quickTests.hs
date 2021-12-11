module QuickTests(
    cesarDoubleEnc,
    cesarCheck,
    subCheck,
    vigenereCheck,
    cesarFullRotation,
) where

import Test.QuickCheck
import Cypher(vigenere, substitute, cesar)


newtype CharList = CharList String deriving (Eq, Show)


vowel :: Gen Char
vowel = elements (['a'..'z'] ++ ['A'..'Z'] ++ [' ', ',', ';', '.', '?', '!', ':', '-', '(', ')'])






cesarDoubleEnc :: CharList -> Int -> Int -> Bool
cesarDoubleEnc msm m n = cesar (cesar (show msm) m True) n True == cesar (show msm) (m+n) True

cesarCheck :: String -> Int -> Bool
cesarCheck msm key = cesar (cesar msm key True) key False == msm

subCheck :: String -> String -> Bool
subCheck msm key = substitute (substitute msm key True) key False == msm  

vigenereCheck :: String -> String -> Bool
vigenereCheck msm key = vigenere (vigenere msm key True) key False == msm

cesarFullRotation :: String -> Bool
cesarFullRotation msm = cesar msm 26 True == msm && cesar msm 26 False == msm 




