module Machine where

import Language.Types
import Util.Assoc
import Util.StatHeap

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

type TiDump = [TiStack]

type TiHeap = StatHeap Node

type TiGlobals = Assoc Name Addr

type TiStats =
  ( Int, -- The number of steps
    ( Int, -- The number of supercombinator reduction
      Int -- The number of primitive reduction
    ),
    Int -- The maximun stack depth
  )

data Node
  = NAp Addr Addr
  | NSc Name [Name] SmolExpr
  | NNum Int
  | NInd Addr
  | NPrim Name Primitive
  | NData Int [Addr]

data Primitive
  = Neg
  | Add
  | Sub
  | Mul
  | Div
  | PrimConstr Int Int
  | If
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | Eq
  | NotEq

initialTiDump :: TiDump
initialTiDump = []

tiStatInitial :: TiStats
tiStatInitial = (0, (0, 0), 0)

tiFinal :: TiState -> Bool
tiFinal ([soleAddr], [], heap, _, _) = isDataNode (statHLookup heap soleAddr)
tiFinal ([], _, _, _, _) = error "Empty stack is dectected"
tiFinal _ = False

primitives :: Assoc Name Primitive
primitives =
  [ ("negate", Neg),
    ("+", Add),
    ("-", Sub),
    ("*", Mul),
    ("/", Div),
    ("if", If),
    (">", Greater),
    (">=", GreaterEq),
    ("<", Less),
    ("<=", LessEq),
    ("==", Eq),
    ("~=", NotEq)
  ]

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode (NData _ _) = True
isDataNode _ = False

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats statFun (stack, dump, heap, scDefs, stats) =
  (stack, dump, heap, scDefs, statFun stats)

doAdmin :: TiState -> TiState
doAdmin state@(stack, _, _, _, stats) =
  applyToStats (updateMaxStackDepth . tiStatIncSteps) state
  where
    updateMaxStackDepth
      | stackDepth <= statMaxStackDepth = id
      | otherwise = tiStatSetMaxStackDepth stackDepth

    stackDepth = length stack
    statMaxStackDepth = tiStatGetMaxStackDepth stats

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps (steps, redStats, maxStackDepth) =
  (steps + 1, redStats, maxStackDepth)

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps (steps, _, _) = steps

tiStatIncScReds :: TiStats -> TiStats
tiStatIncScReds (steps, (scReds, pReds), maxStackDepth) =
  (steps, (scReds + 1, pReds), maxStackDepth)

tiStatGetScReds :: TiStats -> Int
tiStatGetScReds (_, (scReds, _), _) =
  scReds

tiStatIncPReds :: TiStats -> TiStats
tiStatIncPReds (steps, (scReds, pReds), maxStackDepth) =
  (steps, (scReds, pReds + 1), maxStackDepth)

tiStatGetPReds :: TiStats -> Int
tiStatGetPReds (_, (_, pReds), _) =
  pReds

tiStatSetMaxStackDepth :: Int -> TiStats -> TiStats
tiStatSetMaxStackDepth new_max (steps, (scReds, pReds), _) =
  (steps, (scReds, pReds), new_max)

tiStatGetMaxStackDepth :: TiStats -> Int
tiStatGetMaxStackDepth (_, _, maxStackDepth) =
  maxStackDepth

showState :: TiState -> String
showState (stack, _, heap, _, _) = showStack heap stack

showStack :: TiHeap -> TiStack -> String
showStack heap stack =
  "=== STACK ===\n"
    ++ showItems stack
    ++ "=============\n"
    ++ "\n=== HEAP ===\n"
    ++ showHeap heap (showNode heap)
    ++ "\n=============\n\n"
  where
    showItems [] = ""
    showItems (s : rest) =
      showAddr s
        ++ ": "
        ++ showNode heap (statHLookup heap s)
        ++ "\n"
        ++ showItems rest

showStats :: TiState -> String
showStats state@(_, _, heap, _, stats) =
  "======= STATS ========="
    ++ "\nTotal number of steps: "
    ++ show steps
    ++ "\nTotal number of reductions: "
    ++ show (scReds + pReds)
    ++ "\nTotal number of supercombinator reductions: "
    ++ show scReds
    ++ "\nTotal number of primitive reductions: "
    ++ show pReds
    ++ "\nTotal number of heap allocations: "
    ++ show allocations
    ++ "\nTotal number of heap updates: "
    ++ show updates
    ++ "\nMaximum stack depth: "
    ++ show maxStackDepth
    ++ "\n=======================\n"
    ++ showRes state
  where
    steps = tiStatGetSteps stats
    scReds = tiStatGetScReds stats
    pReds = tiStatGetPReds stats
    maxStackDepth = tiStatGetMaxStackDepth stats

    allocations = statHSGetHAlloc hstats
    updates = statHSGetHUpdate hstats
    hstats = statHGetStats heap

showRes :: TiState -> String
showRes (hd : _, _, heap, _, stats) =
  "=== RES ===\n"
    ++ showNode heap (statHLookup heap hd)
    ++ "\n==========="

getRes :: TiState -> Int
getRes (hd : _, _, heap, _, stats) = case statHLookup heap hd of
  NData tag _ -> tag
  NNum n -> n
  _ -> error "Wrong data type for getRes is detected"

showNode :: TiHeap -> Node -> String
showNode _ (NAp a1 a2) = "NAp " ++ showAddr a1 ++ " " ++ showAddr a2
showNode _ (NSc scName argNames _) = "NSc " ++ scName ++ show argNames
showNode _ (NNum n) = "NNum " ++ show n
showNode heap (NInd a) = "NInd (" ++ showNode heap (statHLookup heap a) ++ ")"
showNode _ (NPrim name _) = "NPrim " ++ name
showNode heap (NData tag args) = "NData " ++ show tag ++ ", " ++ show (map (showNode heap . statHLookup heap) args)
