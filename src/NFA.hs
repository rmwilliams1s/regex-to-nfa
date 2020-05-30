{-
 - Compilers conference Spring 2020
 -
 - Create NFAs from fragments, translation
 - for regex operations into fragments.
 -
 - Rachel Williams
 -
 -}

module NFA 
  where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Symbol = Char
type InState = Int
type Transition = M.Map Symbol (S.Set InState)
type StateMap = M.Map InState Transition

data Nfa = Nfa { nfaMoves :: StateMap
               , nfaStart :: InState
               , nfaFinal :: InState
               }
               deriving (Show)
data NfaFrag = NfaFrag { stM :: StateMap
                       , initState :: InState
                       , finalState :: InState
                       , outSym :: Symbol
                       , stateCount :: Int     
                       }
                       deriving (Show)

emptyNfa :: Nfa
emptyNfa = Nfa { nfaMoves = M.empty
               , nfaStart = -1
               , nfaFinal = -1
           }


---------------------------------------------------------------------------
---- fragment construction ----
----------------------------------------------------------------

addToTransition :: Symbol -> InState -> Transition -> Transition
-- given a transition map,  add c==>n to the map
--   so if c was not in it, add c==>{n}
--   if c was mapped to s, then now it is mapped to s union {n}
addToTransition c n t = case M.lookup c t of
    Nothing -> M.insert c (S.singleton n) t
    Just s  -> M.insertWith S.union c (S.singleton n) t


addTransition :: InState -> Symbol -> InState -> StateMap -> StateMap
-- -- given nfa mapping (mapping from states to their transition maps),
-- --   add st0 ==> (c ==> st1) to the map
addTransition st0 c st1 sm = case M.lookup st0 sm of
    Nothing -> M.insert st0 (addToTransition c st1 M.empty) sm
    Just t  -> M.insertWith M.union st0 (addToTransition c st1 t) sm


listToStateMap :: [(InState, Symbol, InState)] -> StateMap
-- transform list of transition triples into a state map
listToStateMap l = case l of
    []            -> M.empty
    ((x,y,z) : s) -> addTransition x y z (listToStateMap s)


renumber :: Int -> StateMap -> StateMap
-- adds k to each state number
renumber k sm = M.mapKeys (+ k) (M.map (renumberT k) sm)


renumberT :: Int -> Transition -> Transition
renumberT k t = M.map (S.map (+ k)) t


---------------------------------------------------------------------------
---- fragment combination functions ----
----------------------------------------------------------------

-- helper functions for readability
epsilon :: Symbol
epsilon = 'ε'

renumFrag :: Int -> NfaFrag -> NfaFrag
renumFrag n nf = nf { stM = renumber n (stM nf)
                    , initState = initState nf + n
                    , finalState = finalState nf + n }

-- NFA fragment operations
singleFrag :: Symbol -> NfaFrag
singleFrag s = NfaFrag { stM = (listToStateMap []) 
                       , initState = 0
                       , finalState = 0
                       , outSym = s
                       , stateCount = 1
               } 

andFrag :: NfaFrag -> NfaFrag -> NfaFrag
andFrag s t = NfaFrag { stM = (addTransition (finalState s) (outSym s) (initState t') (M.union (stM s) (stM t')))
                      , initState = (initState s)
                      , finalState = (finalState t')
                      , outSym = (outSym t')
                      , stateCount = ((stateCount s) + (stateCount t'))
              }
               where t' = renumFrag (stateCount s) t

orFrag :: NfaFrag -> NfaFrag -> NfaFrag
orFrag s t = NfaFrag { stM = (addTransition (finalState s') (outSym s') lstFrag m1)
                     , initState = 0
                     , finalState = lstFrag
                     , outSym = epsilon
                     , stateCount = (2 + (stateCount s) + (stateCount t))
             }
             where lstFrag = 1 + (stateCount s) + (stateCount t)
                   s' = renumFrag 1 s
                   t' = renumFrag (stateCount s' + 1 ) t
                   m1 = addTransition 0 epsilon (initState s') m2
                   m2 = addTransition (finalState t') (outSym t') lstFrag m3
                   m3 = addTransition 0 epsilon (initState t') (M.union (stM s') (stM t'))


starFrag :: NfaFrag -> NfaFrag
starFrag s = NfaFrag { stM = (addTransition 0 epsilon (initState s') m)
                     , initState = 0
                     , finalState = 0
                     , outSym = epsilon
                     , stateCount = (1 + (stateCount s'))
             }
             where s' = renumFrag 1 s
                   m = addTransition (finalState s') (outSym s') 0 (stM s')


---------------------------------------------------------------------------
---- fragment to NFA ----
----------------------------------------------------------------

fragToNfa :: NfaFrag -> Nfa
fragToNfa f = Nfa { nfaMoves = m
                  , nfaStart = (initState f)
                  , nfaFinal = (finalState f + 1)
                  }
            where m = addTransition (finalState f) (outSym f) (finalState f + 1) (stM f)


---------------------------------------------------------------------------
---- Pretty Printing ----
----------------------------------------------------------------

nfaToString :: Nfa -> String
nfaToString nfa = showStart (nfaStart nfa) ++ (stmToString (nfaMoves nfa)) ++ showFinal (nfaFinal nfa)

stmToString :: StateMap -> String
stmToString sm = foldr (++) "" (M.mapWithKey stmToString' sm)

stmToString' :: InState -> Transition -> String
stmToString' k v = (trToString k v) ++ "\n"

trToString :: InState -> Transition -> String
trToString i t = foldr (++) "" (M.mapWithKey (folder i) t)

folder :: InState -> Symbol -> S.Set InState -> String
folder i k v = foldr (++) "" (S.map (trToString' i k) v)

trToString' :: InState -> Symbol -> InState -> String
trToString' ins sym st = (showState ins) ++ "--" ++ (cts sym) ++ "-->" ++ (showState st) ++ " "

showState :: InState -> String
showState s = "( " ++ (show s) ++ " )"

showStart :: InState -> String
showStart s = "\n->( " ++ (show s) ++ " )\n"

showFinal :: InState -> String
showFinal s = "(( " ++ (show s) ++ " ))\n"

cts :: Symbol -> String
cts c = if c == '\949' then "eps" else show c


---------------------------------------------------------------------------
---- testing ----
-----------------------------------------------------------------

example :: [(InState, Symbol, InState)]
example = [(0, 'a', 1), (0, 'a', 2), (1, 'b', 0), (1, 'b', 2), (2, 'a', 0)]

example2 :: [(InState, Symbol, InState)]
example2 = [(0, 'a', 1), (1, 'b', 0)]

-- listToStateMap example
-- fromList [(0,fromList [('a',fromList [1,2])]),
--           (1,fromList [('b',fromList [0,2])]),
--           (2,fromList [('a',fromList [0])])]


-- renumber 5 (listToStateMap example)
-- fromList [(5,fromList [('a',fromList [6,7])]),
--           (6,fromList [('b',fromList [5,7])]),
--           (7,fromList [('a',fromList [5])])]

frag1 :: NfaFrag
frag1 = NfaFrag (listToStateMap example)
                0
                2
                's'
                3

frag2 :: NfaFrag
frag2 = NfaFrag (listToStateMap example2)
                0
                1
                't'
                2


-- andFrag frag1 frag2
-- NfaFrag {stM = [(0, fromList [('a', fromList [1,2])]),
--           (1, fromList [('b', fromList [0,2])]),
--           (2, fromList [('a', fromList [0], 's', fromList [3])]),
--           (3, fromList [('a', fromList [4])]),
--           (4, fromList [('b', fromList [3])])],
--          initState = 0,
--          finalState = 4,
--          outSym = 't',
--          stateCount = 5
--         }

-- orFrag frag1 frag2
-- NfaFrag = {stM = fromList [(0, fromList [('ε', fromList [1,4])]),
--             (1, fromList [('a', fromList [2,3])]),
--             (2, fromList [('b', fromList [1,3])]),
--             (3, fromList [('a', fromList [1], 's', fromList[6])]),
--             (4, fromList [('a', fromList [5])]),
--             (5, fromList [('b', fromList [4], 't', fromList[6])])],
--            initialState = 0,
--            finalState = 6,    
--            outSym ='ε',
--            stateCount = 7
--           }

-- starFrag frag1
-- NfaFrag = {stM = fromList [(0, fromList [('ε', fromList [1])]),
--             (1, fromList [('a', fromList [2,3])]),
--             (2, fromList [('b', fromList [1,3])]),
--             (3, fromList [('a', fromList [1], 's', fromList[0])])],
--           initState = 0,
--           finalState = 0,
--           outSym = 'ε',
--           stateCount = 4
--           }

fragToFull :: NfaFrag
fragToFull = orFrag frag1 frag2

-- fragToNfa fragToFull
-- Nfa { nfaMoves = fromList [(0, fromList [('ε', fromList [1,4])]),
--             (1, fromList [('a', fromList [2,3])]),
--             (2, fromList [('b', fromList [1,3])]),
--             (3, fromList [('a', fromList [1], 's', fromList[6])]),
--             (4, fromList [('a', fromList [5])]),
--             (5, fromList [('b', fromList [4], 't', fromList[6])]),
--             (6, fromList [('ε', fromList [7])])],
--       nfaStart = 0,
--       nfaFinal = 7
--  
-- }