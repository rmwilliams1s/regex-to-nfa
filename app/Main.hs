module Main where


import RegEx
import NFA
import ParseRe
import Translate


bins :: String
bins = "0|1(0|1)*"

complicated :: String
complicated = "((a|b)(a|b)*(short|longlonglonglong)(c|d)(c|d)*)*"

withEps :: String
withEps = "(" ++ aq ++ "*" ++ aq ++ ")*|" ++ e
  where e  = [RegEx.epsilon]
        aq = "(a|" ++ e ++ ")"

bookEx :: String
bookEx = "(a|b)*ac"

main :: IO ()
main = do
    testOne bookEx
    -- testOne bins
    -- testOne complicated
    -- testOne withEps

testOne :: String -> IO ()
testOne s = do
    putStrLn ("parsing: " ++ (show s))
    re <- return (parse s)
    nfa <- return (regToNfa re)
    putStrLn (showNfa nfa)