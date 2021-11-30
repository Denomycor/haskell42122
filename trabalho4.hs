--Cesar :: String -> Int -> String
--Cesar crypt _ _ | crypt == "enc" = 
--                | crypt == "dec" = 

sub :: String -> String
sub str = drop (- (length str)) (as ++ unique str ++ bs)
    where (as, bs) = splitAt 0 [ x | x <- ['a' .. 'z'], x `notElem` (unique str)]

cesar :: Int -> String
cesar x = bs ++ as
    where (as, bs) = splitAt x ['a' .. 'z']

unique :: String -> String
unique xs = [x | (x,y) <- zip xs [0..], x `notElem` take y xs]

--ccrypt :: String -> Int -> String
--ccrypt 