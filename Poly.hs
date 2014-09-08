module Poly where

import Data.Maybe
import Control.Applicative

import Vec

data Poly = Poly { points :: [Vec] } deriving (Show)

data BoundaryPoint = BP { polyParent :: Poly,
                          edgeNumber :: Int,
                          edgeDistance :: Double } -- From 0 to 1, the distance along the edge
                     deriving (Show)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

pairs :: [a] -> [(a, a)]
pairs c = zip c (rotate 1 c)

triples :: [a] -> [(a, a, a)]
triples c = zip3 c (rotate 1 c) (rotate 2 c)

clockwise :: (Vec, Vec, Vec) -> Bool
clockwise (a, b, c) = perp (b-a) `dot` (c-b) >= 0

isConvex :: Poly -> Bool
isConvex = all (not . clockwise) . triples . points

edges :: Poly -> [Edge]
edges = map (uncurry Edge) . pairs . points

edge :: Poly -> Int -> Edge
edge p i = edges p !! i

bpToVec :: BoundaryPoint -> Vec
bpToVec (BP poly edgeno edged) = edgeAlong (edge poly edgeno) edged

-- [a, b]
sliceList :: Int -> Int -> [a] -> [a]
sliceList from to xs = take (to - from + 1) (drop from xs)

sliceListWrap :: Int -> Int -> [a] -> [a]
sliceListWrap from to xs = if from <= to then
                             sliceList from to xs
                           else
                             take (to + 1) xs ++ drop from xs

verticesBetween :: BoundaryPoint -> BoundaryPoint -> [Vec]
verticesBetween (BP poly edge1 _) (BP _ edge2 _) = if edge2 > edge1 then
                                                      sliceListWrap (edge1+1) edge2 (points poly)
                                                   else
                                                      sliceListWrap edge1 (edge2+1) (points poly)

projectThrough :: BoundaryPoint -> Vec -> BoundaryPoint
projectThrough bp@(BP poly _ _) direction =
  if (bpToVec a `distance` point) > (bpToVec b `distance` point) then a else b
  where point = bpToVec bp
        ray = Ray point direction
        [a, b] = collideLineLike poly ray

polyContains :: Poly -> Vec -> Bool
polyContains poly point = length collisions == 1
  where ray = Ray point (Vec 1 1)
        collisions = filter (intersects ray) (edges poly)

collideLineLike :: (LineLike l) => Poly -> l -> [BoundaryPoint]
collideLineLike p l = mapMaybe (\(i, e) -> (BP p i . snd) <$> intersect l e ) indexedEdges
  where indexedEdges = zip [0..] (edges p)

data PolyCollision = PolyCollision {
  aEnter :: BoundaryPoint,
  aExit :: BoundaryPoint,
  bEnter :: BoundaryPoint,
  bExit :: BoundaryPoint
} deriving (Show)

collisionPoints :: PolyCollision -> [Vec]
collisionPoints (PolyCollision a1 a2 b1 b2) = fmap bpToVec [a1, a2, b1, b2]

collidePolyHalf :: Poly -> Poly -> Maybe (BoundaryPoint, BoundaryPoint)
collidePolyHalf a b = case aCollisions of
                       [a1, a2] ->
                         let a1Start = points a !! edgeNumber a1 in
                         Just $ if polyContains a a1Start then (a2, a1) else (a1, a2)
                       _ -> Nothing
  where aCollisions = concatMap (collideLineLike a) (edges b)

collidePoly :: Poly -> Poly -> Maybe PolyCollision
collidePoly a b = do
  (ain, aout) <- collidePolyHalf a b
  (bin, bout) <- collidePolyHalf b a
  return $ PolyCollision ain aout bin bout
