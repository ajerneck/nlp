-------------------------------------------------------------------------
{- |
Module      :  NLP.Tag.Frequency
Copyright   :  (c) Daniel de Kok, Harm Brouwer 2010, 2011, Alexander Jerneck, 2013
License     :  CC-BY, GPLv3
Maintainer  :  alexander.jerneck@gmail.com
Stability   :  experimental

A frequency-based part of speech tagger.

This module is based on Chapter 7 of "Natural Language Processing for
the Working Programmer" by Daniel de Kok and Harm Brouwer, available
here: http://nlpwp.org/book/chap-tagging.xhtml .

-}
-------------------------------------------------------------------------

module NLP.Tag.Frequency where
        
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import NLP.MPQA


type Tkn = T.Text
type Tag = T.Text

data TrainingInstance = TrainingInstance Tkn Tag deriving Show
  
--TODO: these should be read from a config file.
brownTrainFile :: FilePath
brownTrainFile = "/home/alexander/data/nlp/corpora/nlpwp-data/brown-pos-train.txt"
brownTestFile :: FilePath
brownTestFile = "/home/alexander/data/nlp/corpora/nlpwp-data/brown-pos-test.txt"

rsplit :: T.Text -> T.Text -> (T.Text, T.Text)
rsplit sep s = (T.init tkn,tag) where
  (tkn,tag) = T.breakOnEnd sep s

toTrainingInstance :: T.Text -> TrainingInstance
toTrainingInstance s = TrainingInstance tkn tag where
  (tkn, tag) = rsplit "/" s

tagToToken :: Tag -> Token
tagToToken s = Token tkn (Just tag) [] where
  (tkn, tag) = rsplit "/" s

tokenTagFreqs :: [TrainingInstance] -> Map.Map Tkn (Map.Map Tag Int)
tokenTagFreqs = foldl' countWord Map.empty where
  countWord m  (TrainingInstance token tag)= Map.insertWith (countTag tag) token (Map.singleton tag 1) m
  countTag tag _ = Map.insertWith (+) tag 1

tokenMostFreqTag :: Map.Map Tkn (Map.Map Tag Int) -> Map.Map Tkn Tag
tokenMostFreqTag = Map.map mostFreqTag where
  mostFreqTag = fst . Map.foldlWithKey maxTag ("NIL", 0)
  maxTag acc@(maxTag, maxFreq) tag freq
    | freq > maxFreq = (tag, freq)
    | otherwise = acc

trainFreqTagger :: [TrainingInstance] -> Map.Map Tkn Tag
trainFreqTagger = tokenMostFreqTag . tokenTagFreqs

freqTagWord :: Map.Map Tkn Tag -> Tkn -> Maybe Tag
freqTagWord m k = Map.lookup k m

evalTagger :: (Tkn -> Maybe Tag) -> [TrainingInstance] -> (Int, Int, Int)
evalTagger tagFun = foldl' eval (0,0,0) where
  eval (n, c, u) (TrainingInstance token correctTag) = 
      case tagFun token of
        Just tag -> if tag == correctTag then
                      (n+1, c+1, u)
                    else
                      (n+1, c, u)
        Nothing -> (n+1, c, u+1)

baseLineTagger :: Tag -> a -> Maybe Tag
baseLineTagger tag _ = Just tag

-- | Tag using a supplied tagger, backing off to a default supplied tag if it does not find a match.
backoffTagger :: (Tkn -> Maybe Tag) -> Tag -> Tkn -> Maybe Tag
backoffTagger f bt t = let pick = f t in
  case pick of 
    Just tag -> Just tag
    Nothing -> Just bt

applyTagger :: (Tkn -> Maybe Tag) -> [Token] -> [Token]
applyTagger tagFun = map (applyTagger_ tagFun) where
  applyTagger_ tagFun (Token w _ ms) = Token {_word = w, _pos = tagFun w, _modifiers = ms}
  
-- Main functions

readBrown :: FilePath -> IO [TrainingInstance]
readBrown infile = do
  c <- TIO.readFile infile
  return $ map toTrainingInstance $ T.words c

trainTagger :: FilePath -> IO (Tkn -> Maybe Tag)
trainTagger trainFile = do  
  train <- readBrown trainFile
  let model = trainFreqTagger train
  return $ backoffTagger (freqTagWord model) "NN"
