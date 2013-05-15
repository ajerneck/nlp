{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-------------------------------------------------------------------------
{- |
   Module      :  NLP.Email
   Copyright   :  (c) Alexander Jerneck, 2013
   License     :  GPLv3
   Maintainer  :  alexander.jerneck@gmail.com
   Stability   :  experimental
   
   Module to process data from emails in a mbox files.
   
 -}
-------------------------------------------------------------------------

module NLP.Email where
import Utils.Utils

import Control.Monad.Reader
import Data.Char
import Data.List.Split
import Data.MBox
import qualified Data.Text as T

-- | Settings for processing.
data Config a b = Config {
      -- | information string to print to the user.
      info :: String
      -- | function to process the mailbox with.
    , processingFunc :: (a -> b)
      -- | function to save the results with.
    , saveFunc :: (FilePath -> b -> IO ())
      -- | string appended to infile to get outfile.
    , outfile :: FilePath
    }

-- | The 'processAndSave' function processes the mailbox in infile
-- with processingFunc and saves the result using saveFunc to a new
-- filename made from infile and outfile.
processAndSave :: (MonadIO m, MonadReader (Config MBox b) m) => FilePath -> m ()
processAndSave infile = do
  config <- ask

  liftIO $ putStrLn $ info config ++ show infile ++ "..."

  a <- liftIO $ runWithText parseMBox infile

  let results = processingFunc config a
  let outFileName = infile ++ outfile config

  liftIO $ saveFunc config outFileName results

-- | Functions to clean up message headers and body to make it easier
-- for humans to read it in mbox format.

cleanMessage :: Message -> Message
cleanMessage (Message f h b ) = Message {fromLine = f, headers = rmExtraHeaders h, body = cleanBody b }

rmExtraHeaders :: [(T.Text, T.Text)] -> [(T.Text, T.Text)]
rmExtraHeaders = filter (fOr [isDate, isID, isSubject, isInReplyTo, isReferences, isFrom])

cleanBody :: T.Text -> T.Text
cleanBody = T.unlines . rmCruftLines . rmSignature . T.lines

-- | Remove everything after the signature line.
rmSignature :: [T.Text] -> [T.Text]
rmSignature = head . splitOneOf ["--", "--=20"]

-- | Remove Content- type headers and other strange lines.
rmCruftLines :: [T.Text] -> [T.Text]
rmCruftLines = filter (fAnd [notContentHeaders, notStrangeLine])

notContentHeaders :: T.Text -> Bool
notContentHeaders = not . T.isPrefixOf "Content-"

notStrangeLine :: T.Text -> Bool
notStrangeLine x = not $ T.isPrefixOf "--" x && T.all (not . isSpace) x

-- | Functions to get quoted and original lines of email.

quoted :: T.Text -> T.Text
quoted = T.unlines . filter isQuoted . T.lines

isQuoted :: T.Text -> Bool
isQuoted x = ("|" `T.isPrefixOf` x) || (">" `T.isPrefixOf` x)

original :: T.Text -> T.Text
original = T.unlines . filter (not . isQuoted) . T.lines
