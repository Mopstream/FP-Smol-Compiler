module Util.Heap
  ( Heap,
    Addr,
    hInitial,
    hAlloc,
    hUpdate,
    hFree,
    hLookup,
    hAddresses,
    hSize,
    hNull,
    hIsNull,
    showAddr,
  )
where

import Data.List
import Util.Assoc

type Heap a = (Int, [Addr], Assoc Addr a)

type Addr = Int

hInitial :: Heap a
hInitial = (0, [1 ..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, next : free, cts) n = ((size + 1, free, (next, n) : cts), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, cts) addr a = (size, free, (addr, a) : remove cts addr)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) addr = (size - 1, free, remove cts addr)

hLookup :: Heap a -> Addr -> a
hLookup (_, _, cts) addr = aLookup cts addr (error $ "can't find node" ++ showAddr addr ++ " in heap")

hAddresses :: Heap a -> [Addr]
hAddresses (_, _, cts) = [addr | (addr, _) <- cts]

hSize :: Heap a -> Int
hSize (size, _, _) = size

hNull :: Addr
hNull = 0

hIsNull :: Addr -> Bool
hIsNull = (== hNull)

showAddr :: Addr -> String
showAddr addr = "#" ++ show addr

remove :: Assoc Addr a -> Addr -> Assoc Addr a
remove ((addr', a) : cts) addr
  | addr == addr' = cts
  | otherwise = (addr', a) : remove cts addr
remove [] addr = error $ "Attempt to update or free nonexistent address " ++ showAddr addr
