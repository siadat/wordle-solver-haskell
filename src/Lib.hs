module Lib
    ( startGame
    , MasterType(..)
    , GuessType(..)
    , JudgeType(..)
    , judgeWord
    , simpleGuesser
    , betterGuesser
    , betterGuesserAll
    , checkMask
    , maskFunc
    , filterGuessByJudge
    , stringCharFreq
    , matchWord
    , checkFreqs
    , MaskType
    ) where

import qualified Data.Char as DataChar
import qualified System.IO as SystemIO
import qualified Data.Map as DataMap
import qualified Data.List as DataList
import qualified System.Random as SystemRandom

type WordList = [String]

newtype MasterType = Master String
newtype GuessType  = Guess String deriving (Show)
newtype JudgeType  = Judge String deriving (Eq, Show)
newtype ScoredGuessType = ScoredGuess (GuessType, Int)

type History = DataMap.Map GuessType JudgeType
type HistoryList = [(GuessType, JudgeType)]
type Judger  = GuessType -> JudgeType
type Guesser = History -> Maybe GuessType
type Candidate = String

instance Eq GuessType where
  (Guess a) == (Guess b) = a == b

instance Ord GuessType where
  compare (Guess a) (Guess b) = compare a b

perfectMatch :: JudgeType
perfectMatch = Judge "22222"

startGame :: IO ()
startGame = do
  allWords <- readFile "./words.txt"
  let filteredWords = filterWords (lines allWords)
  let history = DataMap.empty

  gen <- SystemRandom.newStdGen
  let zerosAndOnes = SystemRandom.randomRs (0, 10000) gen :: [Int]
  let shuffledWords = map (\ (_, x) -> x) $ DataList.sortBy (\(i, _) (j, _) -> compare (zerosAndOnes !! i) (zerosAndOnes !! j)) $ zip [0..] filteredWords
  putStrLn "Guide:"
  putStrLn "0     = black"
  putStrLn "1     = yellow"
  putStrLn "2     = green"
  putStrLn "22001 = 2 greens, 2 blacks, 1 yellow"
  putStrLn ""

  playTurn shuffledWords history
  putStrLn "Bye"

showHistoryItem :: (GuessType, JudgeType) -> String
showHistoryItem ((Guess g), (Judge j)) = "History | guess: " ++ g ++ " " ++ j

playTurn :: WordList -> History -> IO ()
playTurn filteredWords history = do
  -- mapM (putStrLn) $ map showHistoryItem $ DataMap.toList history
  let newGuess = betterGuesser filteredWords history
  case newGuess of
    Nothing ->
      putStrLn "Nothing"
    Just (Guess g) ->
      putStrLn $ "Try this word: " ++ g
  putStr "Enter colors here: "
  SystemIO.hFlush SystemIO.stdout

  judge <- getLine
  case newGuess of
    Nothing ->
      putStrLn "Nothing found"
    Just g ->
      if (Judge judge) == perfectMatch then
        putStrLn "Found!"
      else
        let newHistory = DataMap.insert g (Judge judge) history
         in playTurn filteredWords newHistory

filterWords :: WordList -> WordList
filterWords ws = map toLowerStr $ filter (\x -> 5 == length x) ws
  where
    toLowerStr :: String -> String
    toLowerStr = map DataChar.toLower

simpleScorer :: WordList -> History -> [ScoredGuessType]
simpleScorer words history =
  let isInHistory w = DataMap.member (Guess w) history
   in map (f isInHistory) $ DataMap.toList history
    where
        f :: (String -> Bool) -> (GuessType, JudgeType) -> ScoredGuessType
        f isIncludedFunc (Guess g, Judge _)
            | isIncludedFunc g == True = ScoredGuess ((Guess g), 0)
            | otherwise                = ScoredGuess ((Guess g), 1)

simpleGuesser :: WordList -> History -> Maybe GuessType
simpleGuesser words history =
  let gs = filter (\w -> not $ DataMap.member (Guess w) history) words
  in case gs of
    g:_ -> Just (Guess g)
    [] -> Nothing

betterGuesser :: WordList -> History -> Maybe GuessType
betterGuesser words history =
  let gs = betterGuesserAll words history
  in case gs of
    g:_ -> Just g
    [] -> Nothing

betterGuesserAll :: WordList -> History -> [GuessType]
betterGuesserAll words history =
  map (\w -> (Guess w)) $ filter (\w -> checkAgainstHistory (DataMap.toList history) w) words

checkAgainstHistory :: HistoryList -> String -> Bool
checkAgainstHistory ((g, j):hs) candida = matchWord g j candida && checkAgainstHistory hs candida
checkAgainstHistory [] candida = True

matchWord :: GuessType -> JudgeType -> Candidate -> Bool
matchWord guess@(Guess gs) judge@(Judge js) candida =
  let eqs = maskFunc guess judge (== '2')
      nqs = maskFunc guess judge (/= '2')
      condGreens = checkMask eqs candida (==)
      condOthers = checkMask nqs candida (/=)
      cFreqs = stringCharFreq (filterGuessByJudge (/= '2') (Guess candida) judge)
      yFreqs = stringCharFreq (filterGuessByJudge (=='1') guess judge)
      bFreqs = stringCharFreq (filterGuessByJudge (=='0') guess judge)
      condFreqs = checkFreqs cFreqs yFreqs bFreqs gs
   in
      condGreens && condOthers && condFreqs

checkFreqs :: CharFreq -> CharFreq -> CharFreq -> String -> Bool
checkFreqs candidaFreqs guessYellowFreqs guessBlackFreqs (g:gs) =
  let
    inCandidate = case DataMap.lookup g candidaFreqs of
      Just count -> count
      Nothing -> 0
    inYellows = case DataMap.lookup g guessYellowFreqs of
      Just count -> count
      Nothing -> 0
    inBlacks = case DataMap.lookup g guessBlackFreqs of
      Just count -> count
      Nothing -> 0
    ok = case (inYellows > 0, inBlacks > 0) of
      (True, True) -> inCandidate == inYellows
      (False, True) -> inCandidate == 0
      (True, False) -> inCandidate >= inYellows
      (False, False) -> True
  in ok && checkFreqs candidaFreqs guessYellowFreqs guessBlackFreqs gs
checkFreqs _ _ _ [] = True

filterGuessByJudge :: (Char -> Bool) -> GuessType -> JudgeType -> String
filterGuessByJudge f (Guess (g:gs)) (Judge (j:js))
  | (f j) = g:(filterGuessByJudge f (Guess gs) (Judge js))
  | otherwise = (filterGuessByJudge f (Guess gs) (Judge js))
filterGuessByJudge f (Guess _) (Judge _) = []

type MaskType = String
checkMask :: MaskType -> String -> (Char -> Char -> Bool) -> Bool
checkMask ('_':ms) (w:ws) f = checkMask ms ws f
checkMask (g:ms) (w:ws) f = (f g w) && checkMask ms ws f
checkMask [] [] _ = True
checkMask _ _ _ = False

maskFunc :: GuessType -> JudgeType -> (Char -> Bool) -> MaskType
maskFunc (Guess (g:gs)) (Judge (j:js)) f =
  let c = (if (f j) then g else '_')
      l = maskFunc (Guess gs) (Judge js) f
   in (c:l)
maskFunc (Guess _) (Judge _) f = ""

type CharFreq = DataMap.Map Char Int

judgeWord :: MasterType -> GuessType -> JudgeType
judgeWord master@(Master m) guess = judgeWordRec master guess (stringCharFreq m) ""
  where
    judgeWordRec :: MasterType -> GuessType -> CharFreq -> [Char] -> JudgeType
    judgeWordRec (Master (m:ms)) (Guess (g:gs)) remaining acc =
      let prependAndContinue c = judgeWordRec (Master ms) (Guess gs) (decrCharFreq g remaining) (c:acc)
      in
        if (DataMap.member g remaining) then
          if g == m then
            prependAndContinue '2'
          else
            prependAndContinue '1'
        else
          prependAndContinue '0'
    judgeWordRec _ _ _ acc = (Judge $ reverse acc)

decrCharFreq :: Char -> CharFreq -> CharFreq
decrCharFreq c freqs =
  let updated = DataMap.insertWith (+) c (-1) $ freqs
  in DataMap.filter (> 0) updated

stringCharFreq :: String -> CharFreq
stringCharFreq word = DataMap.fromList
                    $ Prelude.map (\x->((head x), (length x)))
                    $ DataList.group
                    $ DataList.sort word
