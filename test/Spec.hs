import Test.Hspec
import Lib

import qualified Data.Map as DataMap

main :: IO ()
main = hspec $ do
    describe "judger" $ do
        it "should judge word" $ do
            judgeWord (Master "abcde") (Guess "abcde") `shouldBe` (Judge "22222")
            judgeWord (Master "abcde") (Guess "abxyc") `shouldBe` (Judge "22001")
            judgeWord (Master "a") (Guess "abcde") `shouldBe` (Judge "2")
            judgeWord (Master "abcde") (Guess "a") `shouldBe` (Judge "2")
        it "should guess a word" $ do
            simpleGuesser ["abcde", "abcxy"] DataMap.empty `shouldBe` Just (Guess "abcde")
            simpleGuesser ["abcde", "abcxy"] (DataMap.fromList [(Guess "abcde", Judge "00000")]) `shouldBe` Just (Guess "abcxy")
            simpleGuesser ["abcde", "abcxy"] (DataMap.fromList [(Guess "abcxy", Judge "00000")]) `shouldBe` Just (Guess "abcde")
            simpleGuesser ["abcde", "abcxy"] (DataMap.fromList [(Guess "abcde", Judge "00000"), (Guess "abcxy", Judge "00000")]) `shouldBe` Nothing
