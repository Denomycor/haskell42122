module QuickTests(
    runAllTests
) where
import Test.QuickCheck
import Cypher(vigenere, substitute, cesar)

newtype MsgValid = MsgValid [Char]
newtype KeyValid = KeyValid [Char]
--newtype CharValid = CharValid Char

letters :: [Char]
letters = ['a'..'z'] ++ ['A'..'Z'] ++ " ,;.?!:-()"

genMsgValid :: Gen MsgValid
genMsgValid =  do MsgValid <$> listOf1 (elements letters)

genKeyValid :: Gen KeyValid
genKeyValid = do KeyValid <$> listOf1 (elements ['A'..'Z'])

--genCharValid :: Gen CharValid
--genCharValid = elements letters

--Check: cesar (cesar m) n == cesar m+n
cesarDoubleEnc :: MsgValid -> Int -> Int -> Bool
cesarDoubleEnc msg m n = cesar (cesar (show msg) m True) n True == cesar (show msg) (m+n) True

--Check Enc & Dec
cesarCheck :: MsgValid -> Int -> Bool
cesarCheck msg key = cesar (cesar (show msg) key True) key False == show msg

--Check Enc & Dec
subCheck :: MsgValid -> KeyValid -> Bool
subCheck msg key = substitute (substitute (show msg) (show key) True) (show key) False == show msg

--Check Enc & Dec
vigenereCheck :: MsgValid -> KeyValid -> Bool
vigenereCheck msg key = vigenere (vigenere (show msg) (show key) True) (show key) False == show msg

--Check if a full rotaion of cesar returns it to the inicial msg
cesarFullRotation :: MsgValid -> Bool
cesarFullRotation msg = cesar (show msg) 26 True == show msg && cesar (show msg) 26 False == show msg

--checkSpacesVigenere :: CharValid -> KeyValid -> Bool
--checkSpacesVigenere = 

runAllTests :: IO ()
runAllTests = do
    quickCheck cesarDoubleEnc
    quickCheck cesarCheck
    quickCheck subCheck
    quickCheck vigenereCheck
    quickCheck cesarFullRotation
    --quickCheck vigenereRotation

instance Arbitrary MsgValid where
    arbitrary = genMsgValid

instance Arbitrary KeyValid where
    arbitrary = genKeyValid

--instance Arbitrary CharValid where
-- arbitrary = genCharValid

instance Show MsgValid where
    show (MsgValid x) = show x

instance Show KeyValid where
    show (KeyValid x) = show x
