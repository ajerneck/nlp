--------------------------------------------------------------------------
{- |
Script      :  NLP.Main
Copyright   :  (c) Alexander Jerneck, 2013
License     :  GPLv3
Maintainer  :  alexander.jerneck@gmail.com
Stability   :  experimental

Script to apply the MPQA sentiment scoring to mailboxes.

-}
--------------------------------------------------------------------------

module NLP.Main where

import Data.Email

import NLP.MPQA
import System.IO
import Utils.Utils


main :: IO ()
main = do
  lex <- readLexicon "/home/alexander/data/nlp/lexicon/mpqa/subjclueslen1-HLTEMNLP05.tff"
  let b = pipeline lex docs
  saveAsCSV stdout b

mainDev :: IO [Row]
mainDev = do
  lex <- readLexicon "/home/alexander/data/nlp/lexicon/mpqa/subjclueslen1-HLTEMNLP05.tff"
  let b = pipeline lex docs
  return b

-- main :: IO ()
-- main = do
--   lex <- readLexicon "~/home/alexander/data/nlp/lexicon/mpqa/subjclueslen1-HLTEMNLP05.tff"

-- --I AM HERE: copy from mpqa.hs so that this works with mbox files directly. make a github repo of it soon.

--   let config =  Config { info = "Finding gleps "
--                        , processingFunc = map pipeline
--                        , saveFunc = saveAsCSV stdout
--                        , outfile = "-gleps-out.dat"
--                        }
--   let allFiles = getAllFiles "/home/alexander/data/ma/emails/lists/gentoo.devel/" (isSuffixOf "001")
--   -- run processAndSave in the reader monad transformer so it gets access to config.
--   allFiles >>= mapM_ (\x -> runReaderT (processAndSave x) config)
