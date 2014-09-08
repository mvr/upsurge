{-# LANGUAGE TemplateHaskell #-}

module Vec where

import Control.Lens
import Control.Monad
import Data.Maybe

data Vec = Vec { _x :: Double, _y :: Double} deriving (Show, Eq)
makeLenses ''Vec

instance Num Vec where
  a + b = Vec (a^.x + b^.x) (a^.y + b^.y)
  negate a = Vec (-a^.x) (-a^.y)
  _ * _ = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

dot :: Vec -> Vec -> Double
dot a b = (a^.x * b^.x) + (a^.y * b^.y)

cross :: Vec -> Vec -> Double
cross a b = (a^.x * b^.y) - (a^.y * b^.x)

perp :: Vec -> Vec
perp a = Vec (a^.y) (-a^.x)

scale :: Vec -> Double -> Vec
scale a d = Vec (a^.x * d) (a^.y * d)

norm :: Vec -> Double
norm a = sqrt (a^.x * a^.x + a^.y * a^.y)

distance :: Vec -> Vec -> Double
distance a b = norm (b - a)

data Line = Line { lineStart :: Vec, lineDirection :: Vec }
data Ray  = Ray { rayStart :: Vec, rayDirection :: Vec }
data Edge = Edge { edgeStart :: Vec, edgeEnd :: Vec }

class LineLike l where
  lineAnchors :: l -> (Vec, Vec)
  lineBounds :: l -> (Double, Double)

infinity :: Double
infinity = read "Infinity"

instance LineLike Line where
  lineAnchors (Line a d) = (a, a + d)
  lineBounds _ = (-infinity, infinity)
instance LineLike Ray  where
  lineAnchors (Ray a d) = (a, a + d)
  lineBounds _ = (0, infinity)
instance LineLike Edge where
  lineAnchors (Edge a b) = (a, b)
  lineBounds _ = (0, 1)

lineToEdge :: Line -> Edge
lineToEdge (Line r d) = Edge r (r + d)

edgeAlong :: Edge -> Double -> Vec
edgeAlong (Edge start end) dist = start + (end - start) `scale` dist

intersectRatios :: (Vec, Vec) -> (Vec, Vec) -> Maybe (Double, Double)
intersectRatios (p, r') (q, s') | denom == 0 = Nothing
                                        | otherwise  = Just (t, u)
  where r = r' - p
        s = s' - q
        denom = r `cross` s
        t = ((q-p) `cross` s) / denom
        u = ((q-p) `cross` r) / denom

intersect :: (LineLike l1, LineLike l2) => l1 -> l2 -> Maybe (Double, Double)
intersect e1 e2 = do
  (u, v) <- intersectRatios (lineAnchors e1) (lineAnchors e2)
  let (bot1, top1) = lineBounds e1
      (bot2, top2) = lineBounds e2
  guard $ bot1 <= u && u <= top1
       && bot2 <= v && v <= top2
  return (u, v)

intersects :: (LineLike l1, LineLike l2) => l1 -> l2 -> Bool
intersects l1 l2 = isJust (intersect l1 l2)
