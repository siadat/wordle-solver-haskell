module Lib
    ( startGame
    , MasterType(..)
    , GuessType(..)
    , ColorsType(..)
    , judgeWord
    , simpleGuesser
    , betterGuesser
    , betterGuesserAll
    , checkMask
    , maskFunc
    , filterGuessByColor
    , countCharFreq
    , matchWord
    , checkFreqs
    , MaskType
    ) where

import qualified Data.Char as DataChar
import qualified Data.Maybe as DataMaybe
import qualified System.IO as SystemIO
import qualified Data.Map as DataMap
import qualified Data.List as DataList
import qualified System.Random as SystemRandom

newtype ColorsType  = Colors String deriving (Eq, Show)
newtype GuessType  = Guess String deriving (Show)
newtype MasterType = Master String
newtype ScoredGuessType = ScoredGuess (GuessType, Int)

type CandidateType = String
type CharFreq = DataMap.Map Char Int
type Guesser = History -> Maybe GuessType
type History = DataMap.Map GuessType ColorsType
type HistoryList = [(GuessType, ColorsType)]
type Judger  = GuessType -> ColorsType
type MaskType = String
type WordList = [String]

instance Eq GuessType where
  (Guess a) == (Guess b) = a == b

instance Ord GuessType where
  compare (Guess a) (Guess b) = compare a b

perfectMatch :: ColorsType
perfectMatch = Colors "22222"

startGame :: IO ()
startGame = do
  filteredWords <- filterWords . lines <$> readFile "./words.txt"
  zerosAndOnes <- SystemRandom.randomRs (0, 10000) <$> SystemRandom.newStdGen :: IO [Int]
  let history = DataMap.empty
  let shuffledWords = map snd $ DataList.sortBy (\(i, _) (c, _) -> compare (zerosAndOnes !! i) (zerosAndOnes !! c)) $ zip [0..] filteredWords

  putStrLn "Guide:"
  putStrLn "0     = black"
  putStrLn "1     = yellow"
  putStrLn "2     = green"
  putStrLn "Example:"
  putStrLn "22001 = 2 greens, 2 blacks, 1 yellow"
  putStrLn ""

  playTurn shuffledWords history
  putStrLn "Bye"

showHistoryItem :: (GuessType, ColorsType) -> String
showHistoryItem (Guess g, Colors c) = "History | guess: " ++ g ++ " " ++ c

playTurn :: WordList -> History -> IO ()
playTurn filteredWords history = do
  -- mapM (putStrLn) $ map showHistoryItem $ DataMap.toList history
  let newGuess = betterGuesser filteredWords history
  case newGuess of
    Nothing ->
      putStrLn "Nothing"
    Just (Guess g) ->
      putStr $ "Enter colors for '" ++ g ++ "': "
  SystemIO.hFlush SystemIO.stdout
  colors <- getLine
  case newGuess of
    Nothing ->
      putStrLn "Nothing found"
    Just g ->
      if Colors colors == perfectMatch then
        putStrLn "Found!"
      else
        let newHistory = DataMap.insert g (Colors colors) history
        in  playTurn filteredWords newHistory

filterWords :: WordList -> WordList
filterWords ws = map toLowerStr $ filter (\x -> 5 == length x) ws
  where
    toLowerStr :: String -> String
    toLowerStr = map DataChar.toLower

simpleScorer :: WordList -> History -> [ScoredGuessType]
simpleScorer words history =
  let isInHistory w = DataMap.member (Guess w) history
  in  map (f isInHistory) $ DataMap.toList history
    where
        f :: (String -> Bool) -> (GuessType, ColorsType) -> ScoredGuessType
        f isIncludedFunc (Guess g, Colors _)
            | isIncludedFunc g = ScoredGuess (Guess g, 0)
            | otherwise        = ScoredGuess (Guess g, 1)

simpleGuesser :: WordList -> History -> Maybe GuessType
simpleGuesser words history = fmap Guess (DataList.find (\w -> DataMap.notMember (Guess w) history) words)

betterGuesser :: WordList -> History -> Maybe GuessType
betterGuesser words history = DataMaybe.listToMaybe $ betterGuesserAll words history

betterGuesserAll :: WordList -> History -> [GuessType]
betterGuesserAll words history =
  map Guess $ filter (\w -> checkAgainstHistory w (DataMap.toList history)) words

checkAgainstHistory :: String -> HistoryList -> Bool
checkAgainstHistory candidate = foldl (\acc (g, c) -> acc && matchWord g c candidate) True

matchWord :: GuessType -> ColorsType -> CandidateType -> Bool
matchWord guess@(Guess gs) colors@(Colors cs) candidate =
  let eqs = maskFunc guess colors (== '2')
      nqs = maskFunc guess colors (/= '2')
      condGreens = checkMask eqs candidate (==)
      condOthers = checkMask nqs candidate (/=)
      cFreqs = countCharFreq (filterGuessByColor (/= '2') (Guess candidate) colors)
      yFreqs = countCharFreq (filterGuessByColor (=='1') guess colors)
      bFreqs = countCharFreq (filterGuessByColor (=='0') guess colors)
      condFreqs = checkFreqs cFreqs yFreqs bFreqs gs
  in
      condGreens && condOthers && condFreqs

checkFreqs :: CharFreq -> CharFreq -> CharFreq -> String -> Bool
checkFreqs cFreq yFreq bFreq = foldl f' True
  where
    f' acc g = let inCandidate = DataMaybe.fromMaybe 0 (DataMap.lookup g cFreq)
                   inYellows   = DataMaybe.fromMaybe 0 (DataMap.lookup g yFreq)
                   inBlacks    = DataMaybe.fromMaybe 0 (DataMap.lookup g bFreq)
                   ok = case (inYellows > 0, inBlacks > 0) of
                     (True,   True) -> inCandidate == inYellows
                     (False,  True) -> inCandidate == 0
                     (True,  False) -> inCandidate >= inYellows
                     (False, False) -> True
               in acc && ok

checkMask :: MaskType -> String -> (Char -> Char -> Bool) -> Bool
checkMask ms ws f = length ms == length ws && and (zipWith f' ms ws)
  where f' g w = g == '_' || f g w

filterGuessByColor :: (Char -> Bool) -> GuessType -> ColorsType -> String
filterGuessByColor f (Guess g) (Colors c) = map fst . filter (f . snd) $ zip g c

maskFunc :: GuessType -> ColorsType -> (Char -> Bool) -> MaskType
maskFunc (Guess g) (Colors c) f = zipWith (curry f') g c
  where f' (g, c) = if f c then g else '_'

judgeWord :: MasterType -> GuessType -> ColorsType
judgeWord (Master m) (Guess g) = Colors
                                . fst
                                . foldr toColorDigit ("", countCharFreq m)
                                $ zip m g
  where
    toColorDigit :: (Char, Char) -> (String, CharFreq) -> (String, CharFreq)
    toColorDigit (m, g) (acc, freqs) = (color:acc, decrCharFreq g freqs)
      where
        color :: Char
        color
              | DataMap.notMember g freqs = '0'
              | g == m = '2'
              | otherwise = '1'

decrCharFreq :: Char -> CharFreq -> CharFreq
decrCharFreq c = DataMap.filter (>0) . DataMap.insertWith (+) c (-1)

countCharFreq :: String -> CharFreq
countCharFreq word = DataMap.fromList
                    $ Prelude.map (\x->(head x, length x))
                    $ DataList.group
                    $ DataList.sort word
