module ParseRe
       (
         parse
       ) where

import Data.Char (isDigit, isAlpha)

import RegEx


parse :: String -> RegEx
parse = parse' . reverse

parse' :: String -> RegEx
parse' ""  = error "empty string"
parse' [c] = if c == epsilon
                 then Epsilon
                 else if validChar c
                          then Single c
                          else error ("invalid character " ++ (show c))
parse' s   =
    case altSeek s of
        Just (l, r) -> Alt (parse' l, parse' r)
        Nothing     ->
            case catSeek s of
                Just (l, r) -> Concat (parse' l, parse' r)
                Nothing     ->
                    if head s == '*'
                        then Star (parse' (tail s))
                        else parse' (parenTrim s)

validChar :: Char -> Bool
validChar c = isDigit c || isAlpha c

parenTrim :: String -> String
parenTrim (')':t) = reverse (parenTrim' (reverse t))
    where parenTrim' ('(':t') = t'
          parenTrim' _        = error "missing ("
parenTrim _       = error "missing )"

altSeek :: String -> Maybe (String, String)
altSeek = altSeek' ""
    where altSeek' a s = case s of
              ""    -> Nothing  --- do we need this case?
              --[_]   -> Nothing
              '|':t -> if null a
                           then Nothing    -- really error "misplaced |"  !!!
                           else Just (t, a)
              ')':t -> let (ps, t') = parenMatch 1 ")" t
                       in  altSeek' (a ++ ps) t'
              c:t -> altSeek' (a ++ [c]) t

catSeek :: String -> Maybe (String, String)
catSeek = catSeek' ""
    where catSeek' a s = case s of
              ""    -> Nothing
              [_]   -> Nothing
              ')':t -> let (ps, t') = parenMatch 1 ")" t
                       in  if null t' then Nothing
                                      else Just (t', a ++ ps)
              '*':t -> catSeek' (a ++ "*") t
              c:t   -> Just (t, a ++ [c])

parenMatch :: Integer -> String -> String -> (String, String)
-- depth accumulated-paren-string remainder-paren-string
parenMatch d a ""    = error "unexpected end of input"
parenMatch d a (c:t) = case c of
    ')' -> parenMatch (d + 1) (a ++ [c]) t
    '(' -> if d > 1
               then parenMatch (d - 1) (a ++ [c]) t
               else (a ++ [c], t)
    _   -> parenMatch d (a ++ [c]) t
