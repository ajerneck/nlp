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

import Control.Monad.Reader
import Data.MBox
import Data.List
import NLP.Email
import NLP.MPQA
import Utils.Utils

lexiconFile :: FilePath
lexiconFile = "/home/alexander/data/nlp/lexicon/mpqa/subjclueslen1-HLTEMNLP05.tff"
emailDir :: FilePath
emailDir = "/home/alexander/data/ma/emails/lists/gentoo.devel/"

messageToDoc :: Message -> Document
messageToDoc m@(Message _ _ b) = Document {_identifier=getHeader isID m, _text=original b}

mainEmails :: IO ()
mainEmails = do
  lexicon <- readLexicon lexiconFile
  -- convert an email to a document, only keeping the original text (discarding quoted text)
--  let messageToDoc m@(Message _ _ b) = Document {_identifier=getHeader isID m, _text=original b}
  let config =  Config { info = "Sentiment-scoring using MPQA "
                       , processingFunc = (pipeline lexicon) . map  messageToDoc
                       , saveFunc = saveAsCSVToFile
                       , outfile = "-mpqa-out.dat"
                       }
  let allFiles = getAllFiles emailDir (isSuffixOf "40001")
  -- run processAndSave in the reader monad transformer so it gets access to config.
  allFiles >>= mapM_ (\x -> runReaderT (processAndSave x) config)

mainEmailsDev :: FilePath -> IO [Row]
mainEmailsDev infile = do
  lexicon <- readLexicon lexiconFile
  docs <- runWithText parseMBox infile
  let results = (pipelineKeepText lexicon) $ map messageToDoc $ docs
  return results    
