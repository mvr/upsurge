module Shunt where

import Vec
import Poly

slice :: Poly -> BoundaryPoint -> BoundaryPoint -> (Poly, Poly)
slice (Poly ps) a b = (Poly (start ++ [newA, newB] ++ end), Poly (middle ++ [newB, newA]))
  where start  = take (edge1 + 1) ps
        middle = sliceList (edge1 + 1) edge2 ps
        end    = drop (edge2 + 1) ps
        edge1  = edgeNumber a
        edge2  = edgeNumber b
        newA   = bpToVec a
        newB   = bpToVec b

stripPoints :: PolyCollision -> [Vec]
stripPoints = undefined
