{-
 - Compilers conference Spring 2020
 -
 - UNFINISHED
 -
 - Given a string, traverse NFA and see
 - if string is accepted.
 - Currently unused, will use once matching
 - is implemented.
 -
 - Rachel Williams
 -
 -}

module Match
   where

import Control.Monad.State

import NFA
import NfaState

match :: Nfa -> String -> Bool
match nfa s = (currentState st == nfaFinal nfa) &&
              (restOfString st == "")
              where (_, st) = runState (matchS nfa s) emptyState

matchS :: Nfa -> String -> Matcher ()
matchS nfa s = undefined

-- if queue empty OR currentState is finalState and restOfString == "" then return the "my state" we are in
-- otherwise recursively navigate

