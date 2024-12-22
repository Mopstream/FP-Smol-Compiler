module Language.Parsing.Util where

import Language.Parsing.Lexer (Token)
import Util.Assoc

type Parser a = [Token] -> Assoc a [Token]

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks =
  [ (combine v1 v2, toks2)
    | (v1, toks1) <- p1 toks,
      (v2, toks2) <- p2 toks1
  ]

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks =
  [ (combine v1 v2 v3, toks3)
    | (v1, toks1) <- p1 toks,
      (v2, toks2) <- p2 toks1,
      (v3, toks3) <- p3 toks2
  ]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks =
  [ (combine v1 v2 v3 v4, toks4)
    | (v1, toks1) <- p1 toks,
      (v2, toks2) <- p2 toks1,
      (v3, toks3) <- p3 toks2,
      (v4, toks4) <- p4 toks3
  ]

pIfFail :: Parser a -> Parser a -> Parser a
pIfFail p1 p2 toks =
  case p1 toks of
    res@(_ : _) -> res
    [] -> p2 toks

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pIfFail` pEmpty []

pEmpty :: a -> Parser a
pEmpty v toks = [(v, toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks =
  [ (f v', toks')
    | (v', toks') <- p toks
  ]

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep pV pSep =
  pThen (:) pV (pZeroOrMore (pThen (const id) pSep pV))

pSat :: (String -> Bool) -> Parser String
pSat predicate ((_, tokVal) : toks)
  | predicate tokVal = [(tokVal, toks)]
  | otherwise = []
pSat _ [] = []
