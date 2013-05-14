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

tests = test [ "testReadLexicon" ~: do lex <- readLexicon lexiconFile
                                       assertEqual "for the number of words in lexicon," 6885 (length $ Map.keys lex)
             , "testdocvalues" ~: do lex <- readLexicon lexiconFile
                                     assertEqual "for the values of all docs," [[0,0,0,0,1,0,0,0,0],[1,0,0,0,1,0,0,0,0],[2,0,1,0,3,0,1,1,0],[0,2,0,1,0,3,1,0,1],[0,1,0,0,1,0,0,0,0]] (map (map _ivalue) $ map rmId $ pipeline lex docs)
             ]
        
        