
module Cypher(

) where

isUpperCase :: Char -> Bool
isUpperCase = flip elem ['A'..'Z']

isLowerCase :: Char -> Bool
isLowerCase = flip elem ['a'..'z']

isPunctuation :: Char -> Bool 
isPunctuation = flip elem [' ', ',', ';', '.', '?', '!', ':', '-', '(', ')']

pos :: Char -> [Char] -> Int
pos = posImp 0

posImp :: Int -> Char -> [Char] -> Int
posImp _ _ [] = -1
posImp i c (a1:a) = if c == a1 then i else posImp (i+1) c a

shift :: Char -> [Char] -> Int -> Char
shift c a n = head $ drop ((pos c a + n) `mod` length a) a

toUpper :: Char -> Char
toUpper ch = toEnum (fromEnum ch + (fromEnum 'A' - fromEnum 'a'))

ccript :: Char -> String -> Int -> Char
ccript char abc x | not (isPunctuation char) = shift (if isUpperCase char then toUpper char else char) abc x 
                  | otherwise = char

cesar :: String -> String -> Int -> String
cesar encDec str x = [ccript y ['a'..'z'] (if encDec == "enc" then x else (-1 * x)) | y <- str]