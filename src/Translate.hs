{-
 - Compilers conference Spring 2020
 -
 - Translate parsed regex into NFA
 - fragments, then NFAs.
 -
 - Rachel Williams
 -
 -}

 module Translate 
    where

import NFA
import RegEx

-- fragment translation

regToFrag :: RegEx -> NfaFrag
regToFrag r = case r of
    Epsilon       -> singleFrag (NFA.epsilon)
    Single c      -> singleFrag c
    Alt (l, r)    -> orFrag (regToFrag l) (regToFrag r)
    Concat (l, r) -> andFrag (regToFrag l) (regToFrag r)
    Star e        -> starFrag (regToFrag e)

regToNfa :: RegEx -> Nfa
regToNfa r = fragToNfa (regToFrag r)

showNfa :: Nfa -> String
showNfa nfa = nfaToString nfa
