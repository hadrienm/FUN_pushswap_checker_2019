import Test.Hspec
import PushSwapChecker

main :: IO ()
main = hspec $ do
    describe "Parssing" $ do
        it "test string is number" $ do
            isStringDigit "123" `shouldBe` True
        it "other dummy test" $ do
            isStringDigit "123a" `shouldBe` False
        it "other dummy test" $ do
            isStringDigit "-123" `shouldBe` True
        it "test spilt on" $ do
            splitOn (=='c') ("coucou") `shouldBe` ["ou","ou"]
        it "check right prearg on input" $ do
            checkArray ["sa", "sb"] `shouldBe` True
        it "check wrong prearg on input" $ do
            checkArray ["sr", "sb"] `shouldBe` False
    describe "Check arguments" $ do
        it "operator without number inside arg" $ do
            startCheck ["sa","sb"] ["-"] `shouldBe` False
        it "test prearg is right and arg is wrong" $ do
            startCheck ["sa","sb"] ["a","b"] `shouldBe` False
        it "test prearg is wrong and arg is right" $ do
            startCheck ["sa","sbr"] ["1","2"] `shouldBe` False
        it "test prearg and arg is wrong" $ do
            startCheck ["sa","sbr"] ["a","2"] `shouldBe` False
        it "test prearg is empty" $ do
            startCheck [] ["1","2"] `shouldBe` True
        it "test arg is empty" $ do
            startCheck ["sa","sb"] [] `shouldBe` True
        it "test prearg and arg is empty" $ do
            startCheck [] [] `shouldBe` True
        it "test prearg is empty and arg is wrong" $ do
            startCheck [] ["a","2"] `shouldBe` False
        it "test arg is empty and prearg is wrong" $ do
            startCheck ["sa","sbr"] [] `shouldBe` False
    describe "sort function" $ do
        it "check swapFunction with two or more arguments" $ do
            swapFunction ["1", "2"] `shouldBe` ["2","1"]
        it "check swapFunction with one" $ do
            swapFunction ["2"] `shouldBe` ["2"]
        it "check swapFunction without argument" $ do
            swapFunction [] `shouldBe` []
        it "check rotate function with two or more argument" $ do
            rotateFunction ["1","2","3"] `shouldBe` ["2","3","1"]
        it "check rotate function with two argument" $ do
            rotateFunction ["2","3"] `shouldBe` ["3","2"]
        it "check rotate function with one argument" $ do
            rotateFunction ["2"] `shouldBe` ["2"]
        it "check rotate function without argument" $ do
            rotateFunction [] `shouldBe` []
        it "check rotate reverse function with two or more argument" $ do
            rotateFunction ["1","2","3"] `shouldBe` ["2","3","1"]
        it "check rotate reverse function with two argument" $ do
            rotateFunction ["2","3"] `shouldBe` ["3","2"]
        it "check rotate reverse function with one argument" $ do
            rotateFunction ["2"] `shouldBe` ["2"]
        it "check rotate reverse function without argument" $ do
            rotateFunction [] `shouldBe` []
        it "check push function" $ do
            pushFunction ["1", "2", "3"] ["4", "5", "6"] `shouldBe` ["1", "4", "5", "6"] 
        it "check push function with the first list is empty" $ do
            pushFunction [] ["4", "5", "6"] `shouldBe` ["4", "5", "6"]
        it "check push function with the second list is empty" $ do
            pushFunction ["2","3"] [] `shouldBe` ["2"]
        it "check push function with the two is empty" $ do
            pushFunction [] [] `shouldBe` []
    describe "test pushwap checker" $ do
        it "sort list" $ do
            sortList ["sa", "pb", "pb", "pb", "sa", "pa", "pa", "pa"] ["2", "1", "3", "6", "5", "8"] [] `shouldBe` ["1","2","3","5","6","8"]
        it "unsort list" $ do
            sortList ["sa", "pb", "pb", "pb"] ["2", "1", "3", "6", "5", "8"] [] `shouldBe` ["failed"]
        it "negativ and positive number" $ do
            sortList ["sa","pb","sa","pb","sc","pb","sa","pb","sc","pb","sc","pb","sc","pb","sb","pb","pb","pa","pa","pa","pa","pa","pa","pa","pa","pa","pb","sa","pb","sa","pb","sa","pb","sc","pb","sb","pb","pb","pb","pb","pa","pa","pa","pa","pa","pa","pa","pa","pa","pb","sa","pb","sc","pb","sb","pb","pb","pb","pb","pb","pb","pa","pa","pa","pa","pa","pa","pa","pa","pa","sa"] ["-1","-7","-9","-3","-9","-45","-9999","12"] [] `shouldBe`["12","-9999","-9","-9","-45","-7","-3","-1"]