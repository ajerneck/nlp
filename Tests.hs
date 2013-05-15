{-# LANGUAGE TemplateHaskell, OverloadedStrings, NoMonomorphismRestriction #-}
module Tests where

import qualified Data.Map as Map
import qualified Data.Text as T
import NLP.MPQA
import NLP.Main
import System.IO
import Test.HUnit

docs :: [Document]
docs = [ Document {_identifier = "doc 1", _text = "This is a test string, without negations in the first sentence. In the second there is a match for abandoned, a lexicon word"}
       , Document {_identifier = "doc 2", _text = "abandoned abhor."}
       , Document {_identifier = "doc 3", _text = "Doc 3 has many lexicon words such as truth false negative positive abandoned abjure abhor."}
       , Document {_identifier = "doc 4", _text = "Doc 4 has many lexicon words such as don't truth false negative positive abandoned abjure abhor."}
       , Document {_identifier = "doc 5", _text = "abandoned not abhor."}
       , Document {_identifier = "doc 6", _text = "abandoned not abhor. you are abandoned."}  
       , Document {_identifier = "doc 7", _text = "abandoned not abhor. you are not abound."}
       ]

refDoc6 = [ TextField {_name = "identifier", _tvalue = "doc 6"}
          , IntField {_name = "type.strongsubj.priorpolarity.negative.NOT.you", _ivalue = 1}
          , IntField {_name = "type.strongsubj.priorpolarity.negative.negated", _ivalue = 1}
          , IntField {_name = "type.weaksubj.priorpolarity.negative.NOT.negated", _ivalue = 1}
          , IntField {_name = "type.weaksubj.priorpolarity.negative.NOT.you", _ivalue = 1}
          , IntField {_name = "type.weaksubj.priorpolarity.negative.you", _ivalue = 1}
          ]
refDoc7 = [ TextField {_name = "identifier", _tvalue = "doc 7"}
          , IntField {_name = "type.strongsubj.priorpolarity.negative.NOT.you", _ivalue = 1}
          , IntField {_name = "type.strongsubj.priorpolarity.negative.negated", _ivalue = 1}
          , IntField {_name = "type.weaksubj.priorpolarity.negative.NOT.negated", _ivalue = 1}
          , IntField {_name = "type.weaksubj.priorpolarity.negative.NOT.you", _ivalue = 1}
          , IntField {_name = "type.weaksubj.priorpolarity.positive.negated", _ivalue = 1}
          , IntField {_name = "type.weaksubj.priorpolarity.positive.you", _ivalue = 1}
          ]

-- | Run tests.
runTests = do
  lex <- readLexicon lexiconFile
  runTestTT $ test [ "testReadLexicon" ~: 6885  @=? (length $ Map.keys lex)
             , "test doc 6" ~: refDoc6 @=? (summarizeLexicon lex (docs !! 5))
             , "test doc 7" ~: refDoc7 @=? (summarizeLexicon lex (docs !! 6))
             ]

-- | Functions to display the result of applying the lexicon to docs. Useful for debugging.

main :: IO ()
main = do
  lex <- readLexicon lexiconFile
  let b = pipeline lex docs
  saveAsCSV stdout b

mainDev :: IO [Row]
mainDev = do
  lex <- readLexicon lexiconFile
  let b = pipeline lex docs
  return b
