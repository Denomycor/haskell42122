import Test.QuickCheck
import Cypher(vigenere, substitute, cesar)

newtype MensagemValida = MensagemValida String

cesarDoubleEnc :: String -> Int -> Int -> Bool
cesarDoubleEnc msm m n = cesar (cesar msm m True) n True == cesar msm (m+n) True

cesarCheck :: String -> Int -> Bool
cesarCheck msm key = cesar (cesar msm key True) key False == msm

subCheck :: String -> String -> Bool
subCheck msm key = substitute (substitute msm key True) key False == msm  

vigenereCheck :: String -> String -> Bool
vigenereCheck msm key = vigenere (vigenere msm key True) key False == msm



