
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

shift :: Char -> [Char] -> Int -> Char
shift c a n = head $ drop ((pos c a + n) `mod` length a) a

posImp :: Int -> Char -> [Char] -> Int
posImp _ _ [] = -1
posImp i c (a1:a) = if c == a1 then i else posImp (i+1) c a
