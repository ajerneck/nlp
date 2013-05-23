{-# LANGUAGE TemplateHaskell, OverloadedStrings, NoMonomorphismRestriction #-}
module NLP.MPQA where

import Control.Applicative
import Control.Lens
import Data.List
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO  as TIO
import System.IO

-- | Data structure to handle tokens
data Token = Token {_word :: T.Text, _pos :: Maybe T.Text, _modifiers :: [T.Text]} deriving Show

-- | Data structure to handle documents
data Document = Document {_identifier :: T.Text, _text :: T.Text} deriving Show

-- | Data structure to handle Lexicon
type Lexicon = Map.Map T.Text AnyField

-- | Data structures to handle results
type Row = [AnyField]

data AnyField = IntField {_name :: T.Text, _ivalue :: Int}
              | TextField {_name :: T.Text, _tvalue :: T.Text}
                deriving (Eq, Show)

-- | Data structure to handle printing of Rows
type Header = [T.Text]

-- | Template haskell to make lenses
$(makeLenses ''AnyField)

-- | Functions for operating on Fields
tupleToIntField :: (T.Text, Int) -> AnyField
tupleToIntField (k,v) = IntField {_name = k, _ivalue = v}

-- | Functions for tokenizing texts.

-- | Tokenizer predicates
isNegation :: T.Text -> Bool
isNegation w = T.isSuffixOf "n't" w || elem w ["never","no","nothing","nowhere","noone","none","not","havent","hasnt","hadnt","cant","couldnt","shouldnt","wont","wouldnt","dont","doesnt","didnt","isnt","arent","aint"]

isYou :: T.Text -> Bool
isYou w = w `elem` ["you","you're","you'r"]

-- | List of tokenizers to apply
allTokenizers :: [(T.Text -> Bool,T.Text)]
allTokenizers = [(isNegation, ".negated"), (isYou,".you")]


tokenize' :: Document -> [Token]
tokenize' = map (\w -> Token w Nothing []) . concatMap T.words . makeClauses . normalize . _text

tokenize :: Document -> [Token]
tokenize = map (makeToken Nothing) .  Map.toList . Map.fromListWith (++) . concat . tokenizeByAll allTokenizers . map T.words . makeClauses . normalize . _text where
  makeToken pos (word, mods) = Token {_word=word, _pos=pos, _modifiers = nub mods}
  -- use nub mods because there are many ".NOT" matches for each token.
  
-- | Tokenize a list of clauses by applying a list of tokenizers.
tokenizeByAll :: Applicative f => f (t -> Bool, T.Text) -> f [t] -> f [(t, [T.Text])]
tokenizeByAll tokenizers clauses = tokenizeBy <$> tokenizers <*> clauses

-- | Tokenize a clause using pred, adding modifier to all tokens after the predicate.
tokenizeBy :: (t -> Bool, T.Text) -> [t] -> [(t, [T.Text])]
tokenizeBy (tokenizer, modifier) clause = prefix ++ suffix  where
    (p, s) = break tokenizer clause
    prefix = map (\x -> (x, [".NOT" `T.append` modifier])) p -- mark non-matching tokens as well.
    suffix = map (\x -> (x, [modifier])) s

normalize :: T.Text -> T.Text
normalize = T.toLower

makeClauses :: T.Text -> [T.Text]
makeClauses = T.split (`elem` ".,:;!?")

-- | Functions to summarize lists of tokens into a Row.

applyLexicon :: Lexicon -> Document -> [Row]
applyLexicon lexicon doc = map (map (setValue 1) . codeToken lexicon) $ tokenize doc

setValue :: Int -> AnyField -> AnyField
setValue = set ivalue

codeToken :: Lexicon -> Token -> [AnyField]
codeToken lexicon t = case Map.lookup (_word t) lexicon of
  Nothing -> []
  Just x -> addModifiers (_modifiers t) [x]

addModifiers :: [T.Text] -> [AnyField] -> [AnyField]
addModifiers [] field = field
addModifiers ms field = (\x y -> modifyName (`T.append` x) y) <$> ms <*> field

modifyName :: (T.Text -> T.Text) -> AnyField -> AnyField
modifyName f = name `over` f

summarizeLexicon :: Lexicon -> Document -> Row
summarizeLexicon lexicon doc = docToField doc : fieldFreq ( concat $ applyLexicon lexicon doc)

docToField :: Document -> AnyField
docToField doc = TextField {_name="identifier", _tvalue = _identifier doc}

fieldFreq :: [AnyField] -> [AnyField]
fieldFreq xs = map tupleToIntField $ Map.toList $ Map.fromListWith (+) [(n, v) | (IntField n v) <- xs]

-- | Functions to handle lexicon

makeLexicon :: [T.Text] -> Lexicon
makeLexicon = makeWordMap . map (extractFields . extractWords) where
    extractWords = map (T.split (=='=')) . T.words
    extractFields = foldr (\(n:v:[]) acc -> if keepField n then (n,v):acc else acc) []
    keepField = flip elem ["type","word1","priorpolarity"]

    makeWordMap = Map.fromList . map keyValuePair
    keyValuePair xs = (snd $ head k, IntField {_name=v, _ivalue=0}) where
      k = filter (\x -> fst x == "word1") xs
      v = T.intercalate "." $ map (\(x,y) -> T.intercalate "." [x,y])  $ xs \\ k -- the values are those elements of the list that is not the key.

readLexicon :: FilePath -> IO Lexicon
readLexicon = fmap (makeLexicon . T.lines) . TIO.readFile 

-- Functions to print out a Row in csv format.

fillEmptyRows :: [Row] -> [Row]
fillEmptyRows rs = map (fillEmptyFields (header rs)) rs

-- | Fill a row missing Fields (from Header) with Fields with values of 0. Sort
-- by name to ensure values line up with header in saveAsCSV.
fillEmptyFields :: Header -> Row -> Row
fillEmptyFields hs row = sortBy (comparing _name) $ unionBy sameName row $ map (\n -> IntField {_name=n, _ivalue=0}) hs where
  sameName a b = _name a == _name b

header :: [Row] -> Header
header =  sort . nub . concatMap (map (view name))

rowToCSV :: Row -> T.Text
rowToCSV = T.intercalate ", " . map getValueAsText

getValueAsText :: AnyField -> T.Text
getValueAsText (TextField _ v) = v
getValueAsText (IntField _ v) = T.pack $ show v

saveAsCSV :: Handle -> [Row] -> IO ()
saveAsCSV handle rows = do
  TIO.hPutStrLn handle $ T.intercalate ", " $ header rows
  TIO.hPutStr handle $ T.unlines $ map rowToCSV rows

saveAsCSVToFile :: FilePath -> [Row] -> IO ()
saveAsCSVToFile filename rows = do
  handle <- openFile filename WriteMode
  saveAsCSV handle rows

-- | Run the complete program

pipeline :: Lexicon -> [Document] -> [Row]
pipeline lexicon = fillEmptyRows . map (summarizeLexicon lexicon)

-- | Run pipeline, adding document text as a field.
pipelineKeepText :: Lexicon -> [Document] -> [Row]
pipelineKeepText lexicon docs = zipWith (:) (map documentTextToField docs) $ pipeline lexicon docs

-- | Make a field out of the document text.
documentTextToField :: Document -> AnyField
documentTextToField (Document i t)= TextField {_name = "text", _tvalue=t}

-- | Functions to display rows matching certain criteria.

report :: [Row] -> [[T.Text]]
report = map (map _tvalue . filter (\x -> _name x == "text")) . take 3 . reverse . sortedByIntValue  "type.strongsubj.priorpolarity.negative.you"

reportIDs :: T.Text -> [Row] -> [[T.Text]]
reportIDs n = map (map _tvalue . filter (\x -> _name x == "identifier")) . take 3 . reverse . sortedByIntValue n

minBy :: T.Text -> [Row] -> Row
minBy n = head . sortedByIntValue n

maxBy :: T.Text -> [Row] -> Row
maxBy n = last . sortedByIntValue n

sortedByIntValue :: T.Text -> [Row] -> [Row]
sortedByIntValue n = sortBy $ comparing (getIntValueByName n) 

getIntValueByName :: T.Text -> Row -> Int
getIntValueByName n = head . map _ivalue . filter (\f -> getName f == n)

getName :: AnyField -> T.Text
getName = view name