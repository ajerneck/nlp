-------------------------------------------------------------------------
{- |
Module      :  NLP.Tag.Brill
Copyright   :  (c) Daniel de Kok, Harm Brouwer 2010, 2011, Alexander Jerneck, 2013
License     :  CC-BY, GPLv3
Maintainer  :  alexander.jerneck@gmail.com
Stability   :  experimental

Implements a Brill-tagger.

This module is based on Chapter 7 of "Natural Language Processing for
the Working Programmer" by Daniel de Kok and Harm Brouwer, available
here: http://nlpwp.org/book/chap-tagging.xhtml .

-}
-------------------------------------------------------------------------

module NLP.Tag.Brill where
        
import Control.Monad
import Data.List
import qualified Data.List.Zipper as Zip
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import NLP.MPQA
import NLP.Tag.Frequency
import Utils.Utils

import Debug.Trace


data Replacement = Replacement Tag Tag
                   deriving (Eq, Ord, Show)
                            
data TransformationRule =   
    NextTagRule Replacement Tag
  | PrevTagRule Replacement Tag 
  | SurroundTagRule Replacement Tag Tag  
    deriving (Eq, Ord, Show)

-- | Transformation-based tagging.

rightCursor :: Zip.Zipper a -> Maybe a
rightCursor = Zip.safeCursor . Zip.right

leftCursor :: Zip.Zipper a -> Maybe a
leftCursor z = if Zip.beginp z then 
               Nothing 
             else
               Zip.safeCursor $ Zip.left z

instNextTagRule0 :: Zip.Zipper (Tag, Tag) -> Maybe TransformationRule
instNextTagRule0 z = do
  (_, next) <- rightCursor z
  (correct, incorrect) <- Zip.safeCursor z
  return $ NextTagRule (Replacement incorrect correct) next
  
instPrevTagRule0 :: Zip.Zipper (Tag, Tag) -> Maybe TransformationRule
instPrevTagRule0 z = do
  (_, prev) <- leftCursor z
  (correct, incorrect) <- Zip.safeCursor z
  return $ PrevTagRule (Replacement incorrect correct) prev

instSurroundTagRule0 :: Zip.Zipper (Tag, Tag) -> Maybe TransformationRule
instSurroundTagRule0 z = do
  (_, next) <- rightCursor z
  (_, prev) <- leftCursor z
  (correct, incorrect) <- Zip.safeCursor z
  return $ SurroundTagRule (Replacement incorrect correct) prev next        

instRules0 :: [Zip.Zipper (Tag, Tag) -> Maybe TransformationRule] -> Zip.Zipper (Tag, Tag) -> Map.Map TransformationRule Int
instRules0 funs = Zip.foldlz' applyFuns Map.empty
  where applyFuns s z
          | correct == proposed = s
          | otherwise = foldl (applyFun z) s funs
          where (correct, proposed) = Zip.cursor z
                applyFun z m f = case f z of
                  Nothing -> m
                  Just r -> Map.insertWith' (+) r 1 m 
                                  
initialLearningState :: [TrainingInstance] -> Zip.Zipper (Tag, Tag)
initialLearningState train = Zip.fromList $ zip (correct train) (proposed train)
  where proposed = map tagger . trainTokens
        correct = map (\(TrainingInstance _ tag) -> tag)
        tagger = fromJust . backoffTagger (freqTagWord model) "NN"
        trainTokens = map (\(TrainingInstance token _) -> token)
        model = trainFreqTagger train

sortRules :: Map.Map TransformationRule Int -> [(TransformationRule, Int)]
sortRules = sortBy (\(_,a) (_,b) -> compare b a) . Map.toList

  
scoreRule :: TransformationRule -> Zip.Zipper (Tag, Tag) -> Int
scoreRule r z = nCorrect - nIncorrect
  where (nCorrect, nIncorrect) = scoreRule_ r z
        
scoreRule_ :: TransformationRule -> Zip.Zipper (Tag, Tag) -> (Int, Int)
scoreRule_ r = Zip.foldlz' (scoreElem r) (0,0)
  where scoreElem r s@(nCorrect, nIncorrect) z =
          case ruleApplication r z of
              Just tag -> if tag == correct then
                          (nCorrect + 1, nIncorrect)
                        else
                          (nCorrect, nIncorrect + 1)
              Nothing -> s
          where (correct, _) = Zip.cursor z

selectRule :: [(TransformationRule, Int)] -> Zip.Zipper (Tag, Tag) -> (TransformationRule, Int)
selectRule ((rule,_):xs) z = selectRule_ xs z (rule, scoreRule rule z)

selectRule_ :: [(TransformationRule, Int)] -> Zip.Zipper (Tag, Tag) -> (TransformationRule, Int) -> (TransformationRule, Int)
selectRule_ [] _ best = best
selectRule_ ((rule, correct):xs) z best@(bestRule, bestScore) 
  | bestScore >= correct = best
  | bestScore >= score = selectRule_ xs z best
  | otherwise = selectRule_ xs z (rule, score)
  where score = scoreRule rule z
    
updateState :: TransformationRule -> Zip.Zipper (Tag, Tag) -> Zip.Zipper (Tag, Tag)
updateState r = Zip.fromList . reverse . Zip.foldlz' (update r) []
                where update r xs z = 
                        case ruleApplication r z of
                          Just tag -> (correct, tag):xs
                          Nothing -> e:xs
                        where e@(correct, _) = Zip.cursor z

transformationRules :: [Zip.Zipper (Tag, Tag) -> Maybe TransformationRule] -> Zip.Zipper (Tag, Tag) -> [TransformationRule]
transformationRules funs state = bestRule : transformationRules funs nextState
                                 where (bestRule, _) = selectRule (sortRules $ instRules0 funs state) state
                                       nextState = updateState bestRule state


ruleApplication :: TransformationRule -> Zip.Zipper (Tag, Tag) -> Maybe Tag
ruleApplication (NextTagRule (Replacement old new) next) z = do
  (_, proposed) <- Zip.safeCursor z
  (_, nextProposed) <- rightCursor z
  if proposed == old && nextProposed == next then Just new else Nothing
ruleApplication (PrevTagRule (Replacement old new) next) z = do
  (_, proposed) <- Zip.safeCursor z
  (_, prevProposed) <- leftCursor z
  if proposed == old && prevProposed == next then Just new else Nothing
ruleApplication (SurroundTagRule (Replacement old new) prev next) z = do
  (_, proposed) <- Zip.safeCursor z
  (_, nextProposed) <- rightCursor z
  (_, prevProposed) <- leftCursor z
  if proposed == old && prevProposed == next && nextProposed == next then Just new else Nothing



-- | Functions to apply the Brill tagger

-- | Apply a list of transformation rules to a list of tokens.
applyBrillTagger :: [TransformationRule] -> [Token] -> [Token]
applyBrillTagger rules = Zip.toList . applyRuleToAllTokens'' rules . Zip.fromList

-- | Apply each rule to a zipper of tokens, by recursing over the list of rules.
applyRuleToAllTokens'' :: [TransformationRule] -> Zip.Zipper Token -> Zip.Zipper Token
applyRuleToAllTokens'' [] tokens = tokens
applyRuleToAllTokens'' (r:rs) tokens = applyRuleToAllTokens'' rs (recursivelyApplyRuleToTokens r tokens)

-- | Apply a rule to all the tokens in the zipper by recursing over the elements of the zipper.
recursivelyApplyRuleToTokens :: TransformationRule -> Zip.Zipper Token -> Zip.Zipper Token
recursivelyApplyRuleToTokens rule tokens = case (trace $ "recursivelyApplyRuleToTokens: " ++ show tokens )Zip.endp tokens of
  False -> recursivelyApplyRuleToTokens rule $ Zip.right $ (ruleApplicationWrapper rule tokens)
  True -> Zip.start tokens

-- | update the Zipper with the new element if the rule applied.
ruleApplicationWrapper :: TransformationRule -> Zip.Zipper Token -> Zip.Zipper Token
ruleApplicationWrapper r z = case (trace $ "ruleApplicationWrapper: "  ++ show r ++ show z) ruleApplicationToToken r z of
--  Just x -> (trace $ "replacing " ++ show (Zip.cursor z) ++ " with " ++ show x) Zip.replace x z
  Just x -> (trace $ "before: " ++ show (z) ++ " after: " ++ show (Zip.replace x z)) Zip.replace x z
  Nothing -> z

-- | Apply a rule to the focused element of a zipper.
ruleApplicationToToken :: TransformationRule -> Zip.Zipper Token -> Maybe Token
ruleApplicationToToken (NextTagRule (Replacement old new) next) z = do  
  pt@(Token w proposed ms) <- Zip.safeCursor z 
  nt@(Token _ nextProposed _) <- rightCursor z
  if (trace $ "nexttagrule " ++ show proposed ++ show old ) chkOneMaybe proposed old && chkOneMaybe nextProposed next then Just (Token w (Just new) ms) else Nothing
ruleApplicationToToken (PrevTagRule (Replacement old new) prev) z = do  
  pt@(Token w proposed ms) <- Zip.safeCursor z 
  nt@(Token _ prevProposed _) <- leftCursor z
  if chkOneMaybe proposed old && chkOneMaybe prevProposed prev then Just (Token w (Just new) ms) else Nothing
ruleApplicationToToken (SurroundTagRule (Replacement old new) prev next) z = do  
  pt@(Token w proposed ms) <- Zip.safeCursor z 
  nt@(Token _ nextProposed _) <- rightCursor z
  nt@(Token _ prevProposed _) <- leftCursor z
  if chkOneMaybe proposed old && chkOneMaybe nextProposed next && chkOneMaybe prevProposed prev then Just (Token w (Just new) ms) else Nothing

chkOneMaybe :: (Eq a, Show a) => Maybe a -> a -> Bool
chkOneMaybe (Just a) b = (trace $ "one is just " ++ show a ++ show b) a == b
chkOneMaybe Nothing _ = (trace $ "is nothing") False
