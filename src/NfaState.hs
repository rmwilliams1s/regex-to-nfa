{-
 - Compilers conference Spring 2020
 -
 - UNFINISHED
 -
 - State support for traversing NFA.
 - Currently unused, will use once matching
 - is implemented.
 -
 - Rachel Williams
 -
 -}

module NfaState 
   where

import Control.Monad.State (State, get, put, modify)

import NFA

data NfaState = St 
   { currentState :: InState
   , stack :: [(InState, String)]
   , visited :: [(InState, String)]
   , restOfString :: String
   , nfa :: Nfa
}

type Matcher = State NfaState

emptyState :: NfaState
emptyState = St { currentState = -1
                , stack = []
                , visited = []
                , restOfString = ""
                , nfa = emptyNfa
                }

initializeState :: Nfa -> String -> Matcher ()
initializeState n s = put (St { currentState = 0
                            , stack = []
                            , visited = []
                            , restOfString = s
                            , nfa = n})

pushInState :: InState -> String -> Matcher ()
pushInState i s = modify (\st -> st { stack = (i, s) : stack st })

popInState :: Matcher ()
popInState = modify (\st -> st { stack = tail $ stack st})

updateCurrent :: InState -> Matcher ()
updateCurrent s = modify (\st -> st { currentState = s})

-- TODO: implement
-- beenVisited :: Transition -> Bool
-- beenVisited t = t `elem` (get visited)