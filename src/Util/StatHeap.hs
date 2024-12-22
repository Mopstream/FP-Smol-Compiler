module Util.StatHeap
  ( StatHeap,
    StatHeapStats,
    Addr,
    showAddr,
    showHeap,
    statHInitial,
    statHAlloc,
    statHUpdate,
    statHLookup,
    statHAddresses,
    statHSize,
    statHNull,
    statHIsNull,
    statHGetStats,
    statHSInitial,
    statHSIncHAlloc,
    statHSGetHAlloc,
    statHSIncHUpdate,
    statHSGetHUpdate,
  )
where

import Util.Assoc
import Util.Heap

type StatHeap a = (Heap a, StatHeapStats)

type StatHeapStats =
  ( Int, -- The number of heap allocations
    Int -- The number of heap updates
  )

statHInitial :: StatHeap a
statHInitial = (hInitial, statHSInitial)

statHAlloc :: StatHeap a -> a -> (StatHeap a, Addr)
statHAlloc (heap, stats) n = ((heap', statHSIncHAlloc stats), addr)
  where
    (heap', addr) = hAlloc heap n

statHUpdate :: StatHeap a -> Addr -> a -> StatHeap a
statHUpdate (heap, stats) addr a = (hUpdate heap addr a, statHSIncHUpdate stats)

statHLookup :: StatHeap a -> Addr -> a
statHLookup (heap, _) = hLookup heap

statHAddresses :: StatHeap a -> [Addr]
statHAddresses (heap, _) = hAddresses heap

statHSize :: StatHeap a -> Int
statHSize (heap, _) = hSize heap

statHNull :: Addr
statHNull = 0

statHIsNull :: Addr -> Bool
statHIsNull = (== hNull)

statHGetStats :: StatHeap a -> StatHeapStats
statHGetStats (_, stats) = stats

statHSInitial :: StatHeapStats
statHSInitial = (0, 0)

statHSIncHAlloc :: StatHeapStats -> StatHeapStats
statHSIncHAlloc (a, u) = (a + 1, u)

statHSGetHAlloc :: StatHeapStats -> Int
statHSGetHAlloc (a, _) = a

statHSIncHUpdate :: StatHeapStats -> StatHeapStats
statHSIncHUpdate (a, u) = (a, u + 1)

statHSGetHUpdate :: StatHeapStats -> Int
statHSGetHUpdate (_, u) = u

showHeap :: StatHeap a -> (a -> String) -> String
showHeap ((_, _, assoc), _) = showAssoc assoc showAddr
