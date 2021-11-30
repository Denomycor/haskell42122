--Cesar :: String -> Int -> String
--Cesar crypt _ _ | crypt == "enc" = 
--                | crypt == "dec" = 

sub :: String -> String
sub str = drop (- (length str)) (as ++ str ++ bs)
    where (as, bs) = splitAt 0 [ x | x <- ['a' .. 'z'], x `notElem` str]

cesar :: Int -> String
cesar x = bs ++ as
    where (as, bs) = splitAt x ['a' .. 'z']

perm :: String -> String
perm str = [x | x <- ]

    --ccrypt :: String -> Int -> String
--ccrypt 