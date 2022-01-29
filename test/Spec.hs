import Test.Hspec
import Lib

import qualified Data.Map as DataMap

main :: IO ()
main = hspec $ do
    describe "judger" $ do
        it "should judge word" $ do
            judgeWord (Master "abcde") (Guess "abcde") `shouldBe` (Colors "22222")
            judgeWord (Master "abcde") (Guess "abxyc") `shouldBe` (Colors "22001")
            judgeWord (Master "a") (Guess "abcde") `shouldBe` (Colors "2")
            judgeWord (Master "abcde") (Guess "a") `shouldBe` (Colors "2")
        it "should guess a word (brute-force)" $ do
            simpleGuesser ["abcde", "abcxy"] DataMap.empty `shouldBe` Just (Guess "abcde")
            simpleGuesser ["abcde", "abcxy"] (DataMap.fromList [(Guess "abcde", Colors "00000")]) `shouldBe` Just (Guess "abcxy")
            simpleGuesser ["abcde", "abcxy"] (DataMap.fromList [(Guess "abcxy", Colors "00000")]) `shouldBe` Just (Guess "abcde")
            simpleGuesser ["abcde", "abcxy"] (DataMap.fromList [(Guess "abcde", Colors "00000"), (Guess "abcxy", Colors "00000")]) `shouldBe` Nothing
        it "...stuff..." $ do
            maskFunc (Guess "abc") (Colors "120") (\x -> x == '2') `shouldBe` "_b_"
            maskFunc (Guess "abc") (Colors "120") (\x -> x /= '2') `shouldBe` "a_c"
            checkMask "a__" "abc" (==) `shouldBe` True
            checkMask "b__" "abc" (==) `shouldBe` False
            checkMask "a__" "abc" (/=) `shouldBe` False
            checkMask "b__" "abc" (/=) `shouldBe` True
            filterGuessByColor (=='1') (Guess "abc") (Colors "121") `shouldBe` "ac"
            filterGuessByColor (=='2') (Guess "abc") (Colors "002") `shouldBe` "c"
            countCharFreq "hello" `shouldBe` DataMap.fromList [('h', 1), ('e', 1), ('l', 2), ('o', 1)]

            matchWord (Guess "hello") (Colors "22222") "hello" `shouldBe` True
            matchWord (Guess "abc") (Colors "210") "axb" `shouldBe` True --
            matchWord (Guess "abc") (Colors "210") "abc" `shouldBe` False
            matchWord (Guess "bc") (Colors "11") "ab" `shouldBe` False
            matchWord (Guess "bc") (Colors "11") "cb" `shouldBe` True
        it "should guess a word (better)" $ do

            let twoLetterWithoutRepeat = ["ab", "ba", "ac", "ca", "bc", "cb"]
             in do
                betterGuesser ["a", "b", "c"] DataMap.empty `shouldBe` Just (Guess "a")
                betterGuesser twoLetterWithoutRepeat (DataMap.fromList [(Guess "bc", Colors "10")]) `shouldBe` Just (Guess "ab")
                betterGuesser twoLetterWithoutRepeat (DataMap.fromList [(Guess "bc", Colors "11")]) `shouldBe` Just (Guess "cb")
                betterGuesser twoLetterWithoutRepeat (DataMap.fromList [(Guess "bc", Colors "01")]) `shouldBe` Just (Guess "ca")
                betterGuesser twoLetterWithoutRepeat (DataMap.fromList [(Guess "bc", Colors "02")]) `shouldBe` Just (Guess "ac")
                betterGuesser twoLetterWithoutRepeat (DataMap.fromList [(Guess "bc", Colors "20")]) `shouldBe` Just (Guess "ba")
                betterGuesser twoLetterWithoutRepeat (DataMap.fromList [(Guess "ab", Colors "00")]) `shouldBe` Nothing
                
            let threeLetterWords = [[x, y, z] | x <- ['a'..'c'], y <- ['a'..'c'], z <- ['a'..'c']] --- "aaa", "aab", ...
             in do
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aaa", Colors "000")]) `shouldBe` Just (Guess "bbb")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aaa", Colors "200")]) `shouldBe` Just (Guess "abb")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aaa", Colors "220")]) `shouldBe` Just (Guess "aab")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Colors "000")]) `shouldBe` Just (Guess "ccc")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Colors "100")]) `shouldBe` Just (Guess "cca")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Colors "120")]) `shouldBe` Just (Guess "caa")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Colors "001")]) `shouldBe` Just (Guess "bbc")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Colors "101")]) `shouldBe` Just (Guess "bba")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Colors "200")]) `shouldBe` Just (Guess "acc")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Colors "201")]) `shouldBe` Just (Guess "abc")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Colors "210")]) `shouldBe` Just (Guess "aca")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aab", Colors "211")]) `shouldBe` Just (Guess "aba")
                betterGuesser threeLetterWords (DataMap.fromList [(Guess "aaa", Colors "200"), (Guess "abb", Colors "220")]) `shouldBe` Just (Guess "abc")
