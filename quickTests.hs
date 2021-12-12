module QuickTests(
    cesarDoubleEnc,
    cesarCheck,
    subCheck,
    vigenereCheck,
    cesarFullRotation,
    --vigenereRotation,
) where

import Test.QuickCheck
import Cypher(vigenere, substitute, cesar)

newtype MsmValid = MsmValid [Char]
newtype KeyValid = KeyValid [Char]


genMsmValid :: Gen MsmValid
genMsmValid =  do MsmValid <$> listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ [' ', ',', ';', '.', '?', '!', ':', '-', '(', ')']))

genKeyValid :: Gen KeyValid
genKeyValid = do KeyValid <$> listOf1 (elements ['A'..'Z'])


--Check: cesar (cesar m) n == cesar m+n
cesarDoubleEnc :: MsmValid -> Int -> Int -> Bool
cesarDoubleEnc msm m n = cesar (cesar (show msm) m True) n True == cesar (show msm) (m+n) True

--Check Enc & Dec
cesarCheck :: MsmValid -> Int -> Bool
cesarCheck msm key = cesar (cesar (show msm) key True) key False == show msm

--Check Enc & Dec
subCheck :: MsmValid -> KeyValid -> Bool
subCheck msm key = substitute (substitute (show msm) (show key) True) (show key) False == show msm

--Check Enc & Dec
vigenereCheck :: MsmValid -> KeyValid -> Bool
vigenereCheck msm key = vigenere (vigenere (show msm) (show key) True) (show key) False == show msm

--Check if a full rotaion of cesar returns it to the inicial msm
cesarFullRotation :: MsmValid -> Bool
cesarFullRotation msm = cesar (show msm) 26 True == show msm && cesar (show msm) 26 False == show msm


vigenereRotation :: MsmValid -> Bool
vigenereRotation msm = vigenere (show msm) elements [] True == (show msm)



instance Arbitrary MsmValid where
    arbitrary = genMsmValid

instance Arbitrary KeyValid where
    arbitrary = genKeyValid

instance Show MsmValid where
    show (MsmValid x) = show x

instance Show KeyValid where
    show (KeyValid x) = show x




