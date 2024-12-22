module Language.Parsing.Parser
  ( parse,
  )
where

import Data.Char (isAlpha, isDigit)
import Language.Parsing.Lexer
import Language.Parsing.Util
import Language.Types
import Util.Assoc

parse :: String -> SmolProgram
parse = syntax . clex 0

syntax :: [Token] -> SmolProgram
syntax = takeFirstParse . pProgram
  where
    takeFirstParse ((prog, []) : _) = prog
    takeFirstParse (_ : others) = takeFirstParse others
    takeFirstParse [] = error "Syntax error"

pProgram :: Parser SmolProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser SmolScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where
    mkSc :: Name -> [Name] -> a -> SmolExpr -> SmolScDefn
    mkSc name vars _ expr = (name, vars, expr)

pLit :: String -> Parser String
pLit s = pSat (== s)

pVar :: Parser String
pVar = pSat isVal
  where
    isVal cs@(c : _)
      | cs `elem` keywords = False
      | isAlpha c = True
      | otherwise = False
    isVal [] = False
    keywords :: [String]
    keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pNum :: Parser Int
pNum = pSat isNumber `pApply` read
  where
    isNumber (c : _)
      | isDigit c = True
      | otherwise = False
    isNumber [] = False

pLet :: IsRec -> Parser SmolExpr
pLet isRec = pThen4 (mkLet isRec) (pLit keyword) pDefns (pLit "in") pExpr
  where
    keyword
      | isRec = "letrec"
      | otherwise = "let"
    mkLet :: IsRec -> a -> Assoc Name SmolExpr -> b -> SmolExpr -> SmolExpr
    mkLet is_rec _ defns _ = ELet is_rec defns

pDefns :: Parser (Assoc Name SmolExpr)
pDefns = pOneOrMoreWithSep pDefn (pLit ";")
  where
    pDefn :: Parser (Name, SmolExpr)
    pDefn = pThen3 mkDefn pVar (pLit "=") pExpr
      where
        mkDefn :: Name -> a -> SmolExpr -> (Name, SmolExpr)
        mkDefn name _ expr = (name, expr)

pCase :: Parser SmolExpr
pCase = pThen4 mkCase (pLit "case") pExpr (pLit "of") pAlters
  where
    mkCase :: a -> SmolExpr -> b -> [SmolAlter] -> SmolExpr
    mkCase _ expr _ = ECase expr

pAlters :: Parser [SmolAlter]
pAlters = pOneOrMoreWithSep pAlter (pLit ";")
  where
    pAlter :: Parser SmolAlter
    pAlter = pThen3 mkAlter pPattern (pLit "->") pExpr
      where
        mkAlter :: (Int, [Name]) -> a -> SmolExpr -> SmolAlter
        mkAlter (tag, vars) _ expr = (tag, vars, expr)

pPattern :: Parser (Int, [Name])
pPattern = pThen4 mkPattern (pLit "<") pNum (pLit ">") (pZeroOrMore pVar)
  where
    mkPattern :: a -> Int -> b -> [Name] -> (Int, [Name])
    mkPattern _ tag _ vars = (tag, vars)

pLambda :: Parser SmolExpr
pLambda = pThen4 mkLambda (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr
  where
    mkLambda :: a -> [Name] -> b -> SmolExpr -> SmolExpr
    mkLambda _ vars _ = ELam vars

pConstr :: Parser SmolExpr
pConstr = pThen4 mkConstr (pLit "Pack") (pLit "{") pNums (pLit "}")
  where
    mkConstr :: a -> b -> (Int, Int) -> c -> SmolExpr
    mkConstr _ _ (tag, arity) _ = EConstr tag arity

pNums :: Parser (Int, Int)
pNums = pThen3 mkNums pNum (pLit ",") pNum
  where
    mkNums :: Int -> a -> Int -> (Int, Int)
    mkNums a _ b = (a, b)

data PartialExpr
  = NoOp
  | FoundOp Name SmolExpr

pExpr :: Parser SmolExpr
pExpr =
  pLet recursive
    `pAlt` pLet nonRecursive
    `pAlt` pCase
    `pAlt` pLambda
    `pAlt` pExpr1

pExpr1 :: Parser SmolExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c
  where
    pExpr1c :: Parser PartialExpr
    pExpr1c =
      pThen FoundOp (pLit "|") pExpr1
        `pAlt` pEmpty NoOp

pExpr2 :: Parser SmolExpr
pExpr2 = pThen assembleOp pExpr3 pExpr2c
  where
    pExpr2c :: Parser PartialExpr
    pExpr2c =
      pThen FoundOp (pLit "&") pExpr2
        `pAlt` pEmpty NoOp

pExpr3 :: Parser SmolExpr
pExpr3 = pThen assembleOp pExpr4 pExpr3c
  where
    pExpr3c :: Parser PartialExpr
    pExpr3c =
      pThen FoundOp pRelOp pExpr3
        `pAlt` pEmpty NoOp

    relOps :: [Name]
    relOps = ["<", "<=", "==", "~=", ">=", ">"]

    pRelOp :: Parser Name
    pRelOp = pSat (`elem` relOps)

pExpr4 :: Parser SmolExpr
pExpr4 = pThen assembleOp pExpr5 pExpr4c
  where
    pExpr4c :: Parser PartialExpr
    pExpr4c =
      pThen FoundOp (pLit "+") pExpr4
        `pAlt` pThen FoundOp (pLit "-") pExpr5
        `pAlt` pEmpty NoOp

pExpr5 :: Parser SmolExpr
pExpr5 = pThen assembleOp pExpr6 pExpr5c
  where
    pExpr5c :: Parser PartialExpr
    pExpr5c =
      pThen FoundOp (pLit "*") pExpr5
        `pAlt` pThen FoundOp (pLit "/") pExpr6
        `pAlt` pEmpty NoOp

pExpr6 :: Parser SmolExpr
pExpr6 = pOneOrMore pAExpr `pApply` mkApChain
  where
    mkApChain :: [SmolExpr] -> SmolExpr
    mkApChain (expr : exprs) = foldl EAp expr exprs
    mkApChain [] = error "Compiler Bug mkApChain"

pAExpr :: Parser SmolExpr
pAExpr =
  (pNum `pApply` ENum)
    `pAlt` (pVar `pApply` EVar)
    `pAlt` pConstr
    `pAlt` pThen3 ignoreParen (pLit "(") pExpr (pLit ")")
  where
    ignoreParen _ expr _ = expr

assembleOp :: SmolExpr -> PartialExpr -> SmolExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2
