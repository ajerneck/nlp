-------------------------------------------------------------------------
{- |
Module      :  NLP.Tag.Brill.Rules
Copyright   :  (c) Daniel de Kok, Harm Brouwer 2010, 2011, Alexander Jerneck, 2013
License     :  CC-BY, GPLv3
Maintainer  :  alexander.jerneck@gmail.com
Stability   :  experimental

Learning transformation rules is expensive, so here is a list of the
100 best rules.

This module is based on Chapter 7 of "Natural Language Processing for
the Working Programmer" by Daniel de Kok and Harm Brouwer, available
here: http://nlpwp.org/book/chap-tagging.xhtml .

-}
-------------------------------------------------------------------------

module NLP.Tag.Brill.Rules where

import NLP.Tag.Brill

bestRules = [ NextTagRule (Replacement "TO" "IN") "AT"
            , PrevTagRule (Replacement "NN" "VB") "TO"
            , NextTagRule (Replacement "TO" "IN") "NP"
            , PrevTagRule (Replacement "VBN" "VBD") "PPS"
            , PrevTagRule (Replacement "NN" "VB") "MD"
            , NextTagRule (Replacement "TO" "IN") "PP$"
            , PrevTagRule (Replacement "VBN" "VBD") "NP"
            , PrevTagRule (Replacement "PPS" "PPO") "VB"
            , NextTagRule (Replacement "TO" "IN") "JJ"
            , NextTagRule (Replacement "TO" "IN") "NNS"
            , NextTagRule (Replacement "TO" "IN") "CD"
            , PrevTagRule (Replacement "VBN" "VBD") "PPSS"
            , PrevTagRule (Replacement "VB" "NN") "AT"
            , NextTagRule (Replacement "TO" "IN") "PPO"
            , PrevTagRule (Replacement "NN" "VB") "PPSS"
            , PrevTagRule (Replacement "VBD" "VBN") "BE"
            , PrevTagRule (Replacement "VBD" "VBN") "HVD"
            , PrevTagRule (Replacement "PPS" "PPO") "IN"
            , PrevTagRule (Replacement "VBD" "VBN") "HV"
            , PrevTagRule (Replacement "VBD" "VBN") "HVZ"
            , NextTagRule (Replacement "TO" "IN") "DT"
            , PrevTagRule (Replacement "VBD" "VBN") "BEDZ"
            , PrevTagRule (Replacement "NNS" "VBZ") "PPS"
            , PrevTagRule (Replacement "VB" "NN") "JJ"
            , NextTagRule (Replacement "QL" "AP") "NN"
            , NextTagRule (Replacement "QL" "AP") "NNS"
            , PrevTagRule (Replacement "VBN" "VBD") "WPS"
            , NextTagRule (Replacement "PPS" "PPO") "IN"
            , NextTagRule (Replacement "TO" "IN") "WDT"
            , NextTagRule (Replacement "CS" "WPS") "MD"
            , NextTagRule (Replacement "QL" "RB") ","
            , NextTagRule (Replacement "TO" "IN") "VBG"
            , PrevTagRule (Replacement "VBD" "VBN") "BEZ"
            , PrevTagRule (Replacement "NN" "VB") "*"
            , NextTagRule (Replacement "CS" "WPS") "VBZ"
            , NextTagRule (Replacement "CS" "WPS") "VBD"
            , PrevTagRule (Replacement "VBN" "VBD") "WDT"
            , NextTagRule (Replacement "PPS" "PPO") "."
            , NextTagRule (Replacement "TO" "IN") "DTS"
            , NextTagRule (Replacement "TO" "IN") "DTI"
            , PrevTagRule (Replacement "VBD" "VBN") "BED"
            , PrevTagRule (Replacement "VBD" "VBN") "BEN"
            , NextTagRule (Replacement "TO" "IN") "CS"
            , PrevTagRule (Replacement "CS" "DT") "IN"
            , NextTagRule (Replacement "CS" "WPS") "VB"
            , PrevTagRule (Replacement "VB" "NN") "IN"
            , NextTagRule (Replacement "AP" "QL") "JJR"
            , NextTagRule (Replacement "QL" "AP") "IN"
            , NextTagRule (Replacement "TO" "IN") "AP"
            , PrevTagRule (Replacement "VB" "VBN") "HVZ"
            , PrevTagRule (Replacement "PPS" "PPO") "VBG"
            , NextTagRule (Replacement "TO" "IN") "ABN"
            , PrevTagRule (Replacement "VBD" "VBN") "BER"
            , PrevTagRule (Replacement "VB" "VBN") "HVD"
            , PrevTagRule (Replacement "VB" "NN") "PP$"
            , NextTagRule (Replacement "TO" "IN") "NP$"
            , NextTagRule (Replacement "EX" "RB") "."
            , PrevTagRule (Replacement "HVD" "HVN") "HV"
            , PrevTagRule (Replacement "VB" "VBN") "HV"
            , PrevTagRule (Replacement "NN" "VB") "WPS"
            , NextTagRule (Replacement "CS" "WPS") "BER"
            , NextTagRule (Replacement "CD" "PN") "MD"
            , NextTagRule (Replacement "PP$" "PPO") "IN"
            , NextTagRule (Replacement "NR" "JJ") "NP"
            , NextTagRule (Replacement "EX" "RB") ","
            , NextTagRule (Replacement "CS" "QL") "RB"
            , PrevTagRule (Replacement "VBD" "VBN") "*"
            , NextTagRule (Replacement "QL" "RB") "."
            , NextTagRule (Replacement "PP$" "PPO") "."
            , PrevTagRule (Replacement "CS" "VB") "MD"
            , PrevTagRule (Replacement "VBZ" "NNS") "AT"
            , NextTagRule (Replacement "AP" "QL") "QL"
            , PrevTagRule (Replacement "JJ" "VB") "MD"
            , NextTagRule (Replacement "RBR" "JJR") "NN"
            , NextTagRule (Replacement "AT" "QL") "RBR"
            , PrevTagRule (Replacement "PPS" "PPO") "VBN"
            , NextTagRule (Replacement "CS" "WPS") "HVZ"
            , NextTagRule (Replacement "RBR" "JJR") "NNS"
            , NextTagRule (Replacement "CS" "WPS") "HV"
            , PrevTagRule (Replacement "PPS" "PPO") "VBZ"
            , NextTagRule (Replacement "NN" "VB") "PPO"
            , NextTagRule (Replacement "TO" "IN") "IN"
            , NextTagRule (Replacement "TO" "IN") "PPS"
            , PrevTagRule (Replacement "NNS" "VBZ") "WPS"
            , PrevTagRule (Replacement "PPS" "PPO") "IN"
            , NextTagRule (Replacement "OD" "RB") "VBN"
            , NextTagRule (Replacement "CS" "WPS") "HVD"
            , NextTagRule (Replacement "TO" "IN") "PPSS"
            , NextTagRule (Replacement "PPSS" "PPO") "IN"
            , NextTagRule (Replacement "TO" "IN") "PPO"
            , NextTagRule (Replacement "RB" "QL") "JJR"
            , PrevTagRule (Replacement "CC" "IN") "PN"
            , NextTagRule (Replacement "TO" "IN") "PN"
            , PrevTagRule (Replacement "NN" "VB") "MD*"
            , NextTagRule (Replacement "EX" "RB") "IN"
            , NextTagRule (Replacement "AT" "NP") ")"
            , PrevTagRule (Replacement "HVD" "HVN") "HVD"
            , NextTagRule (Replacement "PPSS" "PPO") "."
            , NextTagRule (Replacement "IN" "CS") "PPSS"
            , PrevTagRule (Replacement "VB" "VBN") "BE"
            ]




-- tenBestRules = [ NextTagRule (Replacement "TO" "IN") "AT"
--             , PrevTagRule (Replacement "NN" "VB") "TO"
--             , NextTagRule (Replacement "TO" "IN") "NP"
--             , PrevTagRule (Replacement "VBN" "VBD") "PPS"
--             , PrevTagRule (Replacement "NN" "VB") "MD"
--             , NextTagRule (Replacement "TO" "IN") "PP$"
--             , PrevTagRule (Replacement "VBN" "VBD") "NP"
--             , PrevTagRule (Replacement "PPS" "PPO") "VB"
--             , NextTagRule (Replacement "TO" "IN") "JJ"
--             , NextTagRule (Replacement "TO" "IN") "NNS"
--             ]
               
--also see brill-rules, there are 100 rules there.
