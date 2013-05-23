{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------
{- |
Script      :  NLP.Tag.Main
Copyright   :  (c) Alexander Jerneck, 2013
License     :  GPLv3
Maintainer  :  alexander.jerneck@gmail.com
Stability   :  experimental

Script to learn Brill tagger rules.

-}
--------------------------------------------------------------------------

module Main where

import NLP.Tag.Frequency
import NLP.Tag.Brill

main = do
  c <- readBrown brownTrainFile
  let rules = take 100 $ transformationRules [instNextTagRule0, instPrevTagRule0, instSurroundTagRule0] $ initialLearningState c      
  mapM_ print rules
