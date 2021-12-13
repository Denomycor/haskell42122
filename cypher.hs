module Cypher(
    vigenere,
    substitute,
    cesar,
) where

import Data.Char(toUpper, toLower, isLetter, isLower, isUpper)

isPunctuation :: Char -> Bool 
isPunctuation = flip elem " ,;.?!:-()"

abcSub :: [Char] -> [Char]
abcSub str = unique [toLower x | x <- str ++ ['a'..'z']]

unique :: [Char] -> [Char]
unique xs = [x | (x,y) <- zip xs [0..], x `notElem` take y xs, isLetter x]

subChar :: [Char] -> Char -> [Char] -> Char
subChar abc1 char abc2 | isUpper char = toUpper $ abc1!!pos (toLower char) abc2
                       | isLower char = abc1!!pos char abc2
                       | otherwise = char

pos :: Char -> [Char] -> Int
pos = core 0
    where core = \i c l -> case l of
            [] -> -1
            (a1:a) -> if c == a1 then i else core (i+1) c a

shift :: Char -> [Char] -> Int -> Char
shift c a n = head $ drop ((pos c a + n) `mod` length a) a

shiftAlpha :: Char -> Int -> Char 
shiftAlpha c n | isUpper c = shift c ['A'..'Z'] n
               | isLower c = shift c ['a'..'z'] n
               | otherwise = c
               
zipKey :: [Char] -> [Int] -> [(Char,Int)]
zipKey [] _ = []
zipKey _ [] = []
zipKey (a1:a) (b1:b) | isPunctuation a1 = (a1, 0) : zipKey a (b1:b)
                     | otherwise = (a1, b1) : zipKey a b

cesar :: [Char] -> Int -> Bool -> [Char]
cesar m k op = [shiftAlpha c $ if op then k else k * (-1) | c <- m] 

substitute :: [Char] -> [Char] -> Bool -> [Char]
substitute msg str op = [subChar key x abc | x <- msg]
           where key = if op then abcSub str else ['a'..'z']
                 abc = if op then ['a'..'z'] else abcSub str

vigenere :: [Char] -> [Char] -> Bool -> [Char]
vigenere m k op = [shiftAlpha c n | (c,n) <- zipKey m key]
    where key = cycle $ map (\x -> pos x ['A'..'Z'] * if op then 1 else -1) k
