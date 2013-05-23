{-# LANGUAGE TemplateHaskell, OverloadedStrings, NoMonomorphismRestriction #-}
module Tests where

import qualified Data.Map as Map
import qualified Data.Text as T
import NLP.MPQA
import NLP.Main
import NLP.Tag.Brill
import NLP.Tag.Brill.Rules
import NLP.Tag.Frequency

import System.IO
import Test.HUnit

-- | Test and reference data for MPQA
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

-- Test and reference data for Tag
tagDocs :: [Document]
tagDocs = [ Document {_identifier = "tagDoc 1", _text = "The weight advantage, plus greater durability of the plastic unit, yields a saving of about one-fifth in shipping."}
       , Document {_identifier = "doc 2", _text = "Its elimination would result in the saving of interest costs, heavy when short-term money rates are high, and in freedom from dependence on credit which is not always available when needed most."}
       ]
          
-- | These reference sentences are not correct in that "saving" is marked as a noun, but I use it to test that the tagging works as it should, even though it is not 100% accurate.
tagRefDoc1 :: [T.Text]
tagRefDoc1 = ["the/AT", "weight/NN", "advantage/NN", "plus/CC", "greater/JJR", "durability/NN", "of/IN", "the/AT", "plastic/NN", "unit/NN", "yields/VBZ", "a/AT", "saving/VBG", "of/IN", "about/IN", "one-fifth/NN", "in/IN", "shipping/VBG"]

tagRefDoc2 :: [T.Text]
tagRefDoc2 = ["its/PP$", "elimination/NN", "would/MD", "result/VB", "in/IN", "the/AT", "saving/VBG", "of/IN", "interest/NN", "costs/NNS", "heavy/JJ", "when/WRB", "short-term/NN", "money/NN", "rates/NNS", "are/BER", "high/JJ", "and/CC", "in/IN", "freedom/NN", "from/IN", "dependence/NN", "on/IN", "credit/NN", "which/WDT", "is/BEZ", "not/*", "always/RB", "available/JJ", "when/WRB", "needed/VBN", "most/QL"]

tagRefDocs = [ tagRefDoc1
             , tagRefDoc2
               ]

-- | Run tests.
runTests = do
  lex <- readLexicon lexiconFile
  tagger <- trainTagger brownTrainFile
  runTestTT $ test [ "testReadLexicon" ~: 6885  @=? (length $ Map.keys lex)
             , "test doc 6" ~: refDoc6 @=? (summarizeLexicon lex (docs !! 5))
             , "test doc 7" ~: refDoc7 @=? (summarizeLexicon lex (docs !! 6))
             , "test tagging docs" ~: (map (map (tokenToTuple . tagToToken)) tagRefDocs) @=? (map (map tokenToTuple) $ applyTaggers tagger tagDocs)
             ]

-- | Functions to display the result of applying the lexicon to docs. Useful for debugging.

tokenToTuple (Token w p _) = (w,p)

--mainTag :: IO [[Token]]
applyTaggers tagger docs = brill_tagged where
  tagdocs = map (applyTagger tagger . tokenize') docs
  brill_tagged = map (applyBrillTagger bestRules) tagdocs  

main :: IO ()
main = do
  lex <- readLexicon lexiconFile
  let b = pipeline lex docs
  saveAsCSV stdout b

--mainDev :: IO [(Document, Row)]
mainDev = do
  lex <- readLexicon lexiconFile
  let b = pipelineKeepText lex docs
  return b
