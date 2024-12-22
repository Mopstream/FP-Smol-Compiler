module Language.Parsing.Lexer
  ( Token,
    clex,
  )
where

import Data.Char (isAlpha, isDigit, isSpace)

type Token = (Int, String)

clex :: Int -> String -> [Token]
clex l ('\r' : '\n' : cs) = clex (l + 1) cs
clex l ('\n' : '\r' : cs) = clex (l + 1) cs
clex l ('\n' : cs) = clex (l + 1) cs
clex l (c : cs) | isSpace c = clex l cs
clex l (c : cs) | isDigit c = (l, numTokVal) : clex l restCs
  where
    numTokVal = c : takeWhile isDigit cs
    restCs = dropWhile isDigit cs
clex l (c : cs) | isAlpha c = (l, varTokVal) : clex l restCs
  where
    varTokVal = c : takeWhile isIdChar cs
    restCs = dropWhile isIdChar cs
clex l ('|' : '|' : cs) = clex l restCs
  where
    restCs = dropWhile (`notElem` "\r\n") cs
clex l (c0 : c1 : cs) | opTokVal `elem` twoCharOps = (l, opTokVal) : clex l cs
  where
    opTokVal = [c0, c1]
clex l (c : cs) = (l, [c]) : clex l cs
clex _ [] = []

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || c == '_'

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]
