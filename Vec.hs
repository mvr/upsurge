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

normalize :: Vec -> Vec
normalize a = a `scale` (recip $ norm a)

distance :: Vec -> Vec -> Double
distance a b = norm (b - a)

data Edge = Edge { edgeStart :: Vec, edgeEnd :: Vec } deriving (Show)

infinity :: Double
infinity = read "Infinity"

edgeAlong :: Edge -> Double -> Vec
edgeAlong (Edge start end) dist = start + (end - start) `scale` dist

intersectRatios :: Edge -> Edge -> Maybe (Double, Double)
intersectRatios (Edge p r') (Edge q s') | denom == 0 = Nothing
                                        | otherwise  = Just (t, u)
  where r = r' - p
        s = s' - q
        denom = r `cross` s
        t = ((q-p) `cross` s) / denom
        u = ((q-p) `cross` r) / denom

intersect :: Edge -> Edge -> Maybe Vec
intersect e1 e2 = do
  (u, v) <- intersectRatios e1 e2
  guard $ 0 <= u && u < 1
       && 0 <= v && v < 1
  return $ edgeAlong e1 u

intersects :: Edge -> Edge -> Bool
intersects l1 l2 = isJust (l1 `intersect` l2)
