-- Test for isTransaction
-- 2016.12.23 - All test passed
module TransactionTest () where
import Transaction (isTransaction, readTransaction, nullTransaction, Transaction (..))


correctDecimalTest = isTransaction "-12.4 Chocolate" == True
correctIntegralTest = isTransaction "+10 Chips" == True
corruptedDecimalTest = isTransaction "+12.A Betu van a tizedesek helyeben" == True
corruptedIntegralTest = isTransaction "+9Osszeer a szoveg a szammal" == True
corruptedDescrTest = isTransaction "+99.54" == True
falseTest1 = isTransaction "asfdagsqdsfas" == False
falseTest2 = isTransaction "22 segg" == False
falseTest3 = isTransaction "+Nincsenek szamok" == False
falseTest4 = isTransaction "+++++++++++" == False


t True = "Passed"
t False = "Failed"


testIsTransaction :: IO ()
testIsTransaction = do
    putStrLn $ "correctDecimalTest [" ++ t correctDecimalTest ++ "]"
    putStrLn $ "correctIntegralTest [" ++ t correctIntegralTest ++ "]"
    putStrLn $ "corruptedDecimalTest [" ++ t corruptedDecimalTest ++ "]"
    putStrLn $ "corruptedIntegralTest [" ++ t corruptedIntegralTest ++ "]"
    putStrLn $ "corruptedDescrTest [" ++ t corruptedDescrTest ++ "]"
    putStrLn $ "falseTest1 [" ++ t falseTest1 ++ "]"
    putStrLn $ "falseTest2 [" ++ t falseTest2 ++ "]"
    putStrLn $ "falseTest3 [" ++ t falseTest3 ++ "]"
    putStrLn $ "falseTest4 [" ++ t falseTest4 ++ "]"
    
    
test1 = readTransaction "-12.4 Chocolate" == Transaction {change = -12.4, description = "Chocolate"}
test2 = readTransaction "+10 Chips" == Transaction {change = 10.0, description = "Chips"}
test3 = readTransaction "+12.A Betu van a tizedesek helyeben" == Transaction {change = 12.0, description = "A Betu van a tizedesek helyeben"}
test4 = readTransaction "+90sszeer a szoveg a szammal" == Transaction {change = 90.0, description = "sszeer a szoveg a szammal"}
test5 = readTransaction "+99.54" == Transaction {change = 99.54, description = "No Description"}
test6 = readTransaction "asfdagsqdsfas" == nullTransaction
test7 = readTransaction "22 segg" == nullTransaction
test8 = readTransaction "+Nincsenek szamok" == nullTransaction
test9 = readTransaction "+++++++++++" == nullTransaction

testReadTransaction :: IO ()
testReadTransaction = do
    putStrLn $ "test1 [" ++ t test1 ++ "]"
    putStrLn $ "test2 [" ++ t test2 ++ "]"
    putStrLn $ "test3 [" ++ t test3 ++ "]"
    putStrLn $ "test4 [" ++ t test4 ++ "]"
    putStrLn $ "test5 [" ++ t test5 ++ "]"
    putStrLn $ "test6 [" ++ t test6 ++ "]"
    putStrLn $ "test7 [" ++ t test7 ++ "]"
    putStrLn $ "test8 [" ++ t test8 ++ "]"
    putStrLn $ "test9 [" ++ t test9 ++ "]"