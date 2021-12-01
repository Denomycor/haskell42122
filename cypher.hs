
module Cypher(
    vigenere,
    cesar,
) where

isUpperCase :: Char -> Bool
isUpperCase = flip elem ['A'..'Z']

isLowerCase :: Char -> Bool
isLowerCase = flip elem ['a'..'z']

isPunctuation :: Char -> Bool 
isPunctuation = flip elem [' ', ',', ';', '.', '?', '!', ':', '-', '(', ')']

toUpper :: Char -> Char
toUpper ch = toEnum (fromEnum ch + (fromEnum 'A' - fromEnum 'a'))

posImp :: Int -> Char -> [Char] -> Int
posImp _ _ [] = -1
posImp i c (a1:a) = if c == a1 then i else posImp (i+1) c a

pos :: Char -> [Char] -> Int
pos = posImp 0

shift :: Char -> [Char] -> Int -> Char
shift c a n = head $ drop ((pos c a + n) `mod` length a) a

shiftAlpha :: Char -> Int -> Char 
shiftAlpha c n 
    | isUpperCase c = shift c ['A'..'Z'] n
    | isLowerCase c = shift c ['a'..'z'] n
    | otherwise = c

zipKey :: [Char] -> [Int] -> [(Char,Int)]
zipKey [] _ = []
zipKey _ [] = []
zipKey (a1:a) (b1:b) 
    | isPunctuation a1 = (a1, 0) : zipKey a (b1:b)
    | otherwise = (a1, b1) : zipKey a b

cesar :: [Char] -> Int -> Bool -> [Char]
cesar m k op = [shiftAlpha c n | (c,n) <- zipKey m key]
    where key = repeat $ if op then k else k*(-1)

vigenere :: [Char] -> [Char] -> Bool -> [Char]
vigenere m k op = [shiftAlpha c n | (c,n) <- zipKey m key]
    where key = cycle $ map (\x -> pos x ['A'..'Z'] * if op then 1 else -1) k
