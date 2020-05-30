module RegEx
       ( RegEx (..)
       , reToString
       , reToStringMaxParens
       , epsilon
       ) where

epsilon :: Char
epsilon = 'ε' 

data RegEx
    = Epsilon
    | Single Char
    | Alt (RegEx, RegEx)
    | Concat (RegEx, RegEx)
    | Star RegEx
    -- deriving Show

reToString :: RegEx -> String
reToString re = case re of
    Epsilon       -> [epsilon]
    Single c      -> [c]
    Alt (l, r)    -> (reToString l) ++ "|" ++ (reToString r)
    Concat (l, r) -> (parenIf isAlt l) ++ (parenIf isAlt r)
    Star e        -> (parenIf (not . isAtom) e) ++ "*"
  where paren s = "(" ++ s ++ ")"
        parenIf p e = if p e then paren (reToString e) else reToString e

-- correct but simple unparsing of regular expressions
-- parentheses placed around every non-atomic expression
reToStringMaxParens :: RegEx -> String
reToStringMaxParens re = case re of
    Epsilon       -> [epsilon]
    Single c      -> [c]
    Alt (l, r)    -> paren ((reToStringMaxParens l) ++ "|" ++ (reToStringMaxParens r))
    Concat (l, r) -> paren ((reToStringMaxParens l) ++ (reToStringMaxParens r))
    Star e        -> paren ((reToStringMaxParens e) ++ "*")
  where paren s = "(" ++ s ++ ")"

isAtom :: RegEx -> Bool
isAtom Epsilon    = True
isAtom (Single _) = True
isAtom _          = False

isAlt :: RegEx -> Bool
isAlt (Alt _) = True
isAlt _       = False
