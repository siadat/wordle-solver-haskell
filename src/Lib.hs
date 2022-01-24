module Lib
    ( startGame
    , MasterType(..)
    , GuessType(..)
    , JudgeType(..)
    , judgeWord
    , simpleGuesser
    ) where

import qualified Data.Char as DataChar
import qualified System.IO as SystemIO
import qualified Data.Map as DataMap
import qualified Data.List as DataList

type WordList = [String]

startGame :: IO ()
startGame = do
    putStr $ "Enter a new challenge: "
    SystemIO.hFlush SystemIO.stdout
    allWords <- readFile "/usr/share/dict/words"
    let filteredWords = filterWords (lines allWords)
    master <- getLine
    let guess = solveWordle (simpleGuesser filteredWords) (judgeWord (Master master))
    case guess of
      Just (Guess g) -> putStrLn $ "Found: " ++ g
      Nothing -> putStrLn $ "No match found"
    putStrLn $ "You entered: " ++ master
    putStrLn $ "GAME OVER"

data MasterType = Master String
data GuessType  = Guess String deriving (Show)
data JudgeType  = Judge String deriving (Eq, Show)

type History = DataMap.Map GuessType JudgeType
type Judger  = GuessType -> JudgeType
type Guesser = History -> Maybe GuessType

instance Eq GuessType where
  (Guess a) == (Guess b) = a == b

instance Ord GuessType where
  compare (Guess a) (Guess b) = compare a b

perfectMatch :: JudgeType
perfectMatch = Judge "22222"

--
-- solver
--
solveWordle :: Guesser -> Judger -> Maybe GuessType
solveWordle guesser judger = solveWordleRec DataMap.empty guesser judger

solveWordleRec :: History -> Guesser -> Judger -> Maybe GuessType
solveWordleRec history guesser judger =
  let guess = guesser history
  in case guess of
    Just g -> let j = judger g
                  h = (DataMap.insert g j history)
              in solveWordleRec h guesser judger
    Nothing -> Nothing

simpleGuesser :: WordList -> History -> Maybe GuessType
simpleGuesser words history =
  let gs = filter (\w -> not $ DataMap.member (Guess w) history) words
  in case gs of
    g:_ -> Just (Guess g)
    [] -> Nothing

toLowerStr :: String -> String
toLowerStr = map DataChar.toLower

filterWords :: WordList -> WordList
filterWords ws = map toLowerStr $ filter (\x -> 5 == length x) ws


--
-- judger
--
type CharFreq = DataMap.Map Char Int

judgeWord :: MasterType -> GuessType -> JudgeType
judgeWord master@(Master m) guess = judgeWordRec master guess (stringCharFreq m) ""

judgeWordRec :: MasterType -> GuessType -> CharFreq -> [Char] -> JudgeType
judgeWordRec (Master (m:ms)) (Guess (g:gs)) remaining acc =
  if (DataMap.member g remaining) then
    if g == m then
      judgeWordRec (Master ms) (Guess gs) (decrCharFreq g remaining) ('2':acc)
    else                                 
      judgeWordRec (Master ms) (Guess gs) (decrCharFreq g remaining) ('1':acc)
  else                                   
    judgeWordRec (Master ms) (Guess gs) (decrCharFreq g remaining) ('0':acc)
judgeWordRec _ _ _ acc = (Judge $ reverse acc)

decrCharFreq :: Char -> CharFreq -> CharFreq
decrCharFreq c freqs =
  let updated = DataMap.insertWith (+) c (-1) $ freqs
  in DataMap.filter (\v -> v > 0) updated

stringCharFreq :: String -> CharFreq
stringCharFreq word = DataMap.fromList
                    $ Prelude.map (\x->((head x), (length x)))
                    $ DataList.group
                    $ DataList.sort word

