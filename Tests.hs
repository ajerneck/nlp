{-# LANGUAGE TemplateHaskell, OverloadedStrings, NoMonomorphismRestriction #-}
module Tests where

import qualified Data.Map as Map
import qualified Data.Text as T
import NLP.MPQA
import NLP.Main
import Test.HUnit

isId x = _name x == "identifier"
getId = filter isId 
rmId = filter (not . isId)

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
