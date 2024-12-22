module Compiler
  ( run,
  )
where

import Data.List
import Language.Parsing.Parser
import Language.Prelude
import Language.Types
import Machine
import Util.Assoc
import Util.StatHeap

run :: String -> String -> IO Int
run program log = eval log $ compile $ parse program

compile :: SmolProgram -> TiState
compile program =
  (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where
    scDefs = program ++ preludeDefs ++ extraPreludeDefs

    (initialHeap, globals) = buildInitialHeap scDefs
    initialStack = [addressOfMain]
    addressOfMain = aLookup globals "main" (error "main is not defined")

buildInitialHeap :: [SmolScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs =
  (heap2, scAddrs ++ primAddrs)
  where
    (heap1, scAddrs) = mapAccumL allocateSc statHInitial scDefs
    (heap2, primAddrs) = mapAccumL allocatePrim heap1 primitives

    allocateSc :: TiHeap -> SmolScDefn -> (TiHeap, (Name, Addr))
    allocateSc heap (name, args, body) = (heap', (name, addr))
      where
        (heap', addr) = statHAlloc heap (NSc name args body)

    allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
    allocatePrim heap (name, prim) = (heap', (name, addr))
      where
        (heap', addr) = statHAlloc heap (NPrim name prim)

eval :: String -> TiState -> IO Int
eval log state = do
  appendFile log $ showState state
  let nextState = doAdmin (step state)
  if tiFinal state
    then do
      appendFile log $ showStats state
      putStrLn $ showRes state
      return $ getRes state
    else eval log nextState

step :: TiState -> TiState
step state@(stack, dump, heap, globals, stats) =
  dispatch (statHLookup heap (head stack))
  where
    dispatch (NNum n) = numStep state n
    dispatch (NAp a1 a2) = apStep state a1 a2
    dispatch (NSc scName argNames body) =
      tiStatIncScReds `applyToStats` scStep state scName argNames body
    dispatch (NInd addr) = indStep state addr
    dispatch (NPrim _ prim) =
      tiStatIncPReds `applyToStats` primStep state prim
    dispatch (NData tag args) = dataStep state tag args

scStep :: TiState -> Name -> [Name] -> SmolExpr -> TiState
scStep (stack, dump, heap, globals, stats) scName argNames body
  | argsLength + 1 <= length stack = (stack', dump, heap', globals, stats)
  | otherwise = error ("Two few arguments are provided to the function " ++ scName)
  where
    stack'@(rootAddr : _) = drop argsLength stack
    heap' = instantiateAndUpdate body rootAddr heap env
    env = argBindings ++ globals
    argBindings = zip argNames (getArgs heap stack)
    argsLength = length argNames

numStep :: TiState -> Int -> TiState
numStep ([_], stack : dump, heap, globals, stats) _ =
  (stack, dump, heap, globals, stats)
numStep (stack, _ : _, heap, _, _) _ =
  error ("Wrong stack is detected : " ++ showStack heap stack)
numStep (_, dump, heap, _, _) _ =
  error ("Wrong dump is detected : " ++ intercalate "\n" (map (showStack heap) dump))

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack@(topAddr : _), dump, heap, globals, stats) a1 a2 =
  case arg of
    NInd a3 -> (stack, dump, makeHeap a3, globals, stats)
    _ -> (a1 : stack, dump, heap, globals, stats)
  where
    makeHeap = statHUpdate heap topAddr . NAp a1
    arg = statHLookup heap a2
apStep _ _ _ = error "Empty stack for application is dectected"

indStep :: TiState -> Addr -> TiState
indStep (_ : stack, dump, heap, globals, stats) addr =
  (addr : stack, dump, heap, globals, stats)
indStep _ _ = error "Empty stack for indirection is detected"

dataStep :: TiState -> Int -> [Addr] -> TiState
dataStep ([_], stack : dump, heap, globals, stats) _ _ =
  (stack, dump, heap, globals, stats)
dataStep (stack, _ : dump, heap, globals, stats) _ _ =
  error ("Wrong stack is detected : " ++ showStack heap stack)
dataStep (_, dump, heap, globals, stats) _ _ =
  error ("Wrong dump is detected : " ++ intercalate "\n" (map (showStack heap) dump))

primStep :: TiState -> Primitive -> TiState
primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state div
primStep state (PrimConstr tag arity) = primConstr state tag arity
primStep state If = primIf state
primStep state Greater = primComp state (>)
primStep state GreaterEq = primComp state (>=)
primStep state Less = primComp state (<)
primStep state LessEq = primComp state (<=)
primStep state Eq = primComp state (==)
primStep state NotEq = primComp state (/=)

primNeg :: TiState -> TiState
primNeg (stack@(_ : _ : _), dump, heap, globals, stats) =
  case arg of
    NNum v -> (negApStack, dump, makeHeap v, globals, stats)
    _
      | isDataNode arg -> error "Negation cannot be applied to other than numbers"
      | otherwise -> ([argAddr], negApStack : dump, heap, globals, stats)
  where
    _ : negApStack@(rootAddr : _) = stack

    makeHeap = statHUpdate heap rootAddr . NNum . negate

    argAddr : _ = getArgs heap stack
    arg = statHLookup heap argAddr
primNeg _ = error "Wrong stack for negate is detected"

primConstr :: TiState -> Int -> Int -> TiState
primConstr (stack, dump, heap, globals, stats) tag arity
  | length stack >= arity + 1 = (stack', dump, heap', globals, stats)
  | otherwise = error "Wrong stack for data type construction is detected"
  where
    stack'@(rootAddr : _) = drop arity stack
    heap' = statHUpdate heap rootAddr (NData tag args)
    args = take arity $ getArgs heap stack

primIf :: TiState -> TiState
primIf (stack@(_ : _ : _ : _ : _), dump, heap, globals, stats) =
  case cond of
    NData 1 [] -> (rootStack, dump, falseHeap, globals, stats)
    NData 2 [] -> (rootStack, dump, trueHeap, globals, stats)
    _
      | isDataNode cond -> error "Wrong data type for if is detected"
      | otherwise -> ([condAddr], ifApStack : dump, heap, globals, stats)
  where
    trueHeap = statHUpdate heap rootAddr (NInd trueAddr)
    falseHeap = statHUpdate heap rootAddr (NInd falseAddr)

    _ : ifApStack = stack
    _ : _ : rootStack = ifApStack
    rootAddr : _ = rootStack

    condAddr : trueAddr : falseAddr : _ = getArgs heap stack
    cond = statHLookup heap condAddr
primIf _ = error "Wrong stack for if is detected"

primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith state f = primDyadic state nodeF
  where
    nodeF (NNum v1) (NNum v2) = NNum (f v1 v2)
    nodeF _ _ = error "Wrong data type for a binary arithmetic operation is detected"

primComp :: TiState -> (Int -> Int -> Bool) -> TiState
primComp state f = primDyadic state nodeF
  where
    nodeF (NNum v1) (NNum v2)
      | f v1 v2 = NData 2 []
      | otherwise = NData 1 []
    nodeF _ _ = error "Wrong data type for a binary comparison operation is detected"

primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
primDyadic (stack@(_ : _ : _ : _), dump, heap, globals, stats) f
  | arg1IsDataNode && arg2IsDataNode = (ap2Stack, dump, heap', globals, stats)
  | arg2IsDataNode = ([arg1Addr], ap1Stack : dump, heap, globals, stats)
  | otherwise = ([arg2Addr], ap2Stack : dump, heap, globals, stats)
  where
    heap' = statHUpdate heap rootAddr (f arg1 arg2)

    _ : ap1Stack = stack
    _ : ap2Stack = ap1Stack
    rootAddr : _ = ap2Stack

    arg1Addr : arg2Addr : _ = getArgs heap stack
    arg1 = statHLookup heap arg1Addr
    arg2 = statHLookup heap arg2Addr
    arg1IsDataNode = isDataNode arg1
    arg2IsDataNode = isDataNode arg2
primDyadic _ _ = error "Wrong stack for a binary operation is detected"

instantiate :: SmolExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiate (ENum n) heap env = statHAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env =
  statHAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env =
  (heap, aLookup env v (error ("Undefined name " ++ v)))
instantiate (EConstr tag arity) heap env =
  instantiateConstr tag arity heap env
instantiate (ELet isRec defs body) heap env =
  instantiateLet isRec defs body heap env
instantiate (ECase e alts) heap env =
  error "Can't instantiate case exprs"

instantiateDef :: TiGlobals -> TiHeap -> (Name, SmolExpr) -> (TiHeap, (Name, Addr))
instantiateDef env heap (name, body) =
  (heap', (name, addr))
  where
    (heap', addr) = instantiate body heap env

instantiateLet :: IsRec -> Assoc Name SmolExpr -> SmolExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiateLet isRec defs body heap env = instantiate body heap' env'
  where
    (heap', defBindings) = mapAccumL allocateDef heap defs
    allocateDef = instantiateDef (if isRec then env' else env)
    env' = defBindings ++ env

instantiateAndUpdate :: SmolExpr -> Addr -> TiHeap -> TiGlobals -> TiHeap
instantiateAndUpdate (EAp e1 e2) updateAddr heap env =
  statHUpdate heap2 updateAddr (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiateAndUpdate (ENum n) updateAddr heap env = statHUpdate heap updateAddr (NNum n)
instantiateAndUpdate (EVar v) updateAddr heap env =
  statHUpdate heap updateAddr (NInd vAddr)
  where
    vAddr = aLookup env v (error ("Undefined name " ++ v))
instantiateAndUpdate (EConstr tag arity) updateAddr heap env =
  instantiateAndUpdateConstr tag arity updateAddr heap env
instantiateAndUpdate (ELet isRec defs body) updateAddr heap env =
  instantiateAndUpdateLet isRec defs body updateAddr heap env
instantiateAndUpdate (ECase e alts) updateAddr heap env =
  error "Can't instantiate case exprs"

instantiateAndUpdateLet :: IsRec -> Assoc Name SmolExpr -> SmolExpr -> Addr -> TiHeap -> TiGlobals -> TiHeap
instantiateAndUpdateLet isRec defs body addr heap env = instantiateAndUpdate body addr heap' env'
  where
    (heap', defBindings) = mapAccumL allocateDef heap defs
    allocateDef = instantiateDef (if isRec then env' else env)
    env' = defBindings ++ env

instantiateConstr :: Int -> Int -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiateConstr tag arity heap env = (heap', addr)
  where
    (heap', addr) = statHAlloc heap (NPrim "Pack" (PrimConstr tag arity))

instantiateAndUpdateConstr :: Int -> Int -> Addr -> TiHeap -> TiGlobals -> TiHeap
instantiateAndUpdateConstr tag arity addr heap env = heap'
  where
    heap' = statHUpdate heap addr (NPrim "Pack" (PrimConstr tag arity))

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (_ : stack) =
  map getArg stack
  where
    getArg a =
      case statHLookup heap a of
        NAp _ arg -> arg
        _ -> error "Cannot get arg from non-application node"
getArgs _ _ = error "Cannot get args from empty stack"
