{-# LANGUAGE TemplateHaskell, OverloadedStrings, NoMonomorphismRestriction #-}
module Tests where

import qualified Data.Map as Map
import qualified Data.Text as T
import NLP
import Test.HUnit

isId x = _name x == "identifier"
getId = filter isId 
rmId = filter (not . isId)

tests = test [ "testReadLexicon" ~: do lex <- readLexicon
                                       assertEqual "for the number of words in lexicon," 6885 (length $ Map.keys lex)
             , "testdocvalues" ~: do lex <- readLexicon
                                     assertEqual "for the values of all docs," [[0,1,0,0,0,1],[0,2,0,0,1,1],[0,5,1,2,3,5],[8,5,1,2,3,5],[2,2,0,0,1,1]] (map (map _ivalue) $ map rmId $ pipeline lex docs)
             ]
        
        