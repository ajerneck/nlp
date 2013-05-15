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
import System.IO
import Utils.Utils

lexiconFile = "/home/alexander/data/nlp/lexicon/mpqa/subjclueslen1-HLTEMNLP05.tff"
emailDir = "/home/alexander/data/ma/emails/lists/gentoo.devel/"

mainEmails :: IO ()
mainEmails = do
  lex <- readLexicon lexiconFile
  -- convert an email to a document, only keeping the original text (discarding quoted text)
  let messageToDoc m@(Message f h b) = Document {_identifier=getHeader isID m, _text=original b}
  let config =  Config { info = "Sentiment-scoring using MPQA "
                       , processingFunc = (pipeline lex) . map  messageToDoc
                       , saveFunc = saveAsCSVToFile
                       , outfile = "-mpqa-out.dat"
                       }
  let allFiles = getAllFiles emailDir (isSuffixOf "40001")
  -- run processAndSave in the reader monad transformer so it gets access to config.
  allFiles >>= mapM_ (\x -> runReaderT (processAndSave x) config)
