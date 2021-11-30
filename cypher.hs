
isUpperCase :: Char -> Bool
isUpperCase c = c `elem` ['A', 'B'..'Z']

isLowerCase :: Char -> Bool
isLowerCase c = c `elem` ['a', 'b'..'z']

isPunctuation :: Char -> Bool
isPunctuation c = c `elem` [' ', ',', ';', '.', '?', '!', ':', '-', '(', ')']

posImp :: Int -> Char -> [Char] -> Int
posImp _ _ [] = -1
posImp i c (a1:a) = if c == a1 then i else posImp (i+1) c a

pos :: Char -> [Char] -> Int
pos = posImp 0

shift :: Char -> [Char] -> Int -> Char
shift c a n = head $ drop ((pos c a + n) `mod` length a) a

ccript :: Int -> String -> String
ccript x str = [shift y ['a'..'z'] x | y <- str]

cesar :: String -> Int -> String -> String
cesar encDec x str = if encDec == "enc" then ccript x str else ccript (x * (-1)) str

