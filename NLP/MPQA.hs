{-# LANGUAGE TemplateHaskell, OverloadedStrings, NoMonomorphismRestriction #-}
module NLP.MPQA where

import Control.Lens
import Data.Char
import Data.Email
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO  as TIO
import System.IO
import Utils.Utils

-- | Data structures to handle tokens

data Token = Token {_word :: T.Text, _pos :: T.Text, _modifiers :: Row} deriving Show
--data Modifier = Negated | You | I deriving Show

-- | Data structure to handle documents
data Document = Document {_identifier :: T.Text, _text :: T.Text} deriving Show

-- | Data structures to handle Lexicon

type Lexicon = Map.Map T.Text Row

-- | Data structures to handle results

type Row = [AnyField]

data AnyField = IntField {_name :: T.Text, _ivalue :: Int}
              | TextField {_name :: T.Text, _tvalue :: T.Text}
                deriving Show

-- | Data structures to handle printing of Rows
type Header = [T.Text]

-- -- | Template haskell to make lenses
-- $(makeLenses ''Token)
-- $(makeLenses ''Document)
$(makeLenses ''AnyField)

-- | Functions for operating on Fields

tupleToTextField (k,v) = TextField {_name = k, _tvalue = v}
tupleToIntField (k,v) = IntField {_name = k, _ivalue = v}

tupleToFieldName (k,v) = IntField {_name = T.intercalate "." [k, v], _ivalue =0}

-- | Functions for tokenizing texts.

tokenize :: Document -> [Token]
tokenize = map (\x -> makeToken x "" []). T.words . T.filter (not . isPunctuation) . normalize .  _text

negationTokenize :: Document -> [Token]
negationTokenize = concatMap (tokenizeBy isNegation) . clauses . normalize . _text

isNegation w = T.isSuffixOf "n't" w || elem w ["never","no","nothing","nowhere","noone","none","not","havent","hasnt","hadnt","cant","couldnt","shouldnt","wont","wouldnt","dont","doesnt","didnt","isnt","arent","aint"]

tokenizeBy pred xs = prefix ++ suffix where
    (p, s) = break pred $ T.words xs
    prefix = map (\x -> makeToken x "" []) p -- make tuples with False for the words before the negation.
    suffix = map (\x -> makeToken x "" [IntField {_name="negated.TRUE", _ivalue=1}]) s -- make tuples with True for negated words. 

normalize = T.toLower
clauses = T.split (`elem` ".,:;!?")


makeToken t p ms = Token {_word = t, _pos = p, _modifiers = ms}

-- | Functions to summarize lists of tokens into a Row.

applyLexicon :: Lexicon -> Document -> [Row]
applyLexicon lex doc = map (map (setValue 1) . codeToken lex) $ negationTokenize doc
setValue = set ivalue

codeToken lex t = (_modifiers t) ++ Map.findWithDefault [] (_word t) lex


summarizeLexicon :: Lexicon -> Document -> Row
summarizeLexicon lex doc = docToField doc : fieldFreq ( concat $ applyLexicon lex doc)

docToField doc = TextField {_name="identifier", _tvalue = _identifier doc}

summarizeWordCount = wordCount . map _word

wordCount xs = IntField {_name = "wordcount", _ivalue = length xs}

freq xs = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- xs]

fieldFreq xs = map tupleToIntField $ Map.toList $ Map.fromListWith (+) [(n, v) | (IntField n v) <- xs]

-- | Functions to handle lexicon

makeLexicon :: [T.Text] -> Lexicon
makeLexicon = makeWordMap . map (extractFields . extractWords) where
    extractWords = map (T.split (=='=')) . T.words
    extractFields = foldr (\a@(n:v:[]) acc -> if keepField n then (n,v):acc else acc) []
    keepField = flip elem ["type","word1","priorpolarity"]

    makeWordMap = Map.fromList . map keyValuePair
    keyValuePair xs = (snd $ head k, map tupleToFieldName v) where
        k = filter (\x -> fst x == "word1") xs
        v = xs \\ k -- the values are those elements of the list that is not the key.

readLexicon = fmap (makeLexicon . T.lines) . TIO.readFile 

-- Functions to print out a Row in csv format.

fillEmptyRows :: [Row] -> [Row]
fillEmptyRows rs = map (fillEmptyFields (header rs)) rs

-- | Fill a row missing Fields (from Header) with Fields with values of 0. Sort
-- by name to ensure values line up with header in saveAsCSV.
fillEmptyFields :: Header -> Row -> Row
fillEmptyFields hs row = sortBy (comparing _name) $ unionBy sameName row $ map (\n -> IntField {_name=n, _ivalue=0}) hs
sameName a b = _name a == _name b

header :: [Row] -> Header
header =  sort . nub . concatMap (map (view name))

rowToCSV :: Row -> T.Text
rowToCSV = T.intercalate ", " . map getValueAsText

getValueAsText (TextField n v) = v
getValueAsText (IntField n v) = T.pack $ show v

saveAsCSV :: Handle -> [Row] -> IO ()
saveAsCSV handle rows = do
  TIO.hPutStrLn handle $ T.intercalate ", " $ header rows
  TIO.hPutStr handle $ T.unlines $ map rowToCSV rows

-- | Testing 

pipeline :: Lexicon -> [Document] -> [Row]
pipeline lex = fillEmptyRows . map (summarizeLexicon lex)

docs :: [Document]
docs = [ Document {_identifier = "doc 1", _text = "This is a test string, without negations in the first sentence. In the second there is a match for abandoned, a lexicon word"}
       , Document {_identifier = "doc 2", _text = "abandoned abhor."}
       , Document {_identifier = "doc 3", _text = "Doc 3 has many lexicon words such as truth false negative positive abandoned abjure abhor."}
       , Document {_identifier = "doc 4", _text = "Doc 4 has many lexicon words such as don't truth false negative positive abandoned abjure abhor."}
       , Document {_identifier = "doc 5", _text = "abandoned not abhor."}
         
       ]

  
  
