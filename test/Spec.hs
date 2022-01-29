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
        it "should guess a word (brute-force)" $ do
            simpleGuesser ["abcde", "abcxy"] DataMap.empty `shouldBe` Just (Guess "abcde")
            simpleGuesser ["abcde", "abcxy"] (DataMap.fromList [(Guess "abcde", Judge "00000")]) `shouldBe` Just (Guess "abcxy")
            simpleGuesser ["abcde", "abcxy"] (DataMap.fromList [(Guess "abcxy", Judge "00000")]) `shouldBe` Just (Guess "abcde")
            simpleGuesser ["abcde", "abcxy"] (DataMap.fromList [(Guess "abcde", Judge "00000"), (Guess "abcxy", Judge "00000")]) `shouldBe` Nothing
        it "...stuff..." $ do
            maskFunc (Guess "abc") (Judge "120") (\x -> x == '2') `shouldBe` "_b_"
            maskFunc (Guess "abc") (Judge "120") (\x -> x /= '2') `shouldBe` "a_c"
            checkMask "a__" "abc" (==) `shouldBe` True
            checkMask "b__" "abc" (==) `shouldBe` False
            checkMask "a__" "abc" (/=) `shouldBe` False
            checkMask "b__" "abc" (/=) `shouldBe` True
            filterGuessByJudge (=='1') (Guess "abc") (Judge "121") `shouldBe` "ac"
            filterGuessByJudge (=='2') (Guess "abc") (Judge "002") `shouldBe` "c"
            stringCharFreq "hello" `shouldBe` DataMap.fromList [('h', 1), ('e', 1), ('l', 2), ('o', 1)]

            matchWord (Guess "hello") (Judge "22222") "hello" `shouldBe` True
            matchWord (Guess "abc") (Judge "210") "axb" `shouldBe` True --
            matchWord (Guess "abc") (Judge "210") "abc" `shouldBe` False
            matchWord (Guess "bc") (Judge "11") "ab" `shouldBe` False
            matchWord (Guess "bc") (Judge "11") "cb" `shouldBe` True
        it "should guess a word (better)" $ do

            let twoLetterWithoutRepeat = ["ab", "ba", "ac", "ca", "bc", "cb"]
             in do
                betterGuesser ["a", "b", "c"] DataMap.empty `shouldBe` Just (Guess "a")
                betterGuesser twoLetterWithoutRepeat (DataMap.fromList [(Guess "bc", Judge "10")]) `shouldBe` Just (Guess "ab")
                betterGuesser twoLetterWithoutRepeat (DataMap.fromList [(Guess "bc", Judge "11")]) `shouldBe` Just (Guess "cb")
                betterGuesser twoLetterWithoutRepeat (DataMap.fromList [(Guess "bc", Judge "01")]) `shouldBe` Just (Guess "ca")
                betterGuesser twoLetterWithoutRepeat (DataMap.fromList [(Guess "bc", Judge "02")]) `shouldBe` Just (Guess "ac")
                betterGuesser twoLetterWithoutRepeat (DataMap.fromList [(Guess "bc", Judge "20")]) `shouldBe` Just (Guess "ba")
                betterGuesser twoLetterWithoutRepeat (DataMap.fromList [(Guess "ab", Judge "00")]) `shouldBe` Nothing
                
            let threeLetterWords = [[x, y, z] | x <- ['a'..'c'], y <- ['a'..'c'], z <- ['a'..'c']] --- "aaa", "aab", ...
             in do
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aaa", Judge "000")]) `shouldBe` Just (Guess "bbb")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aaa", Judge "200")]) `shouldBe` Just (Guess "abb")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aaa", Judge "220")]) `shouldBe` Just (Guess "aab")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Judge "000")]) `shouldBe` Just (Guess "ccc")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Judge "100")]) `shouldBe` Just (Guess "cca")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Judge "120")]) `shouldBe` Just (Guess "caa")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Judge "001")]) `shouldBe` Just (Guess "bbc")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Judge "101")]) `shouldBe` Just (Guess "bba")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Judge "200")]) `shouldBe` Just (Guess "acc")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Judge "201")]) `shouldBe` Just (Guess "abc")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Judge "210")]) `shouldBe` Just (Guess "aca")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Judge "211")]) `shouldBe` Just (Guess "aba")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aaa", Judge "200"), (Guess "abb", Judge "220")]) `shouldBe` Just (Guess "abc")
