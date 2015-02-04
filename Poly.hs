module Poly where

import Data.Maybe

import Vec

data Poly = Poly { points :: [Vec] } deriving (Show)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs | n >= 0 = zipWith const (drop n (cycle xs)) xs
rotate n xs | n < 0  = zipWith const (drop (length xs + n) (cycle xs)) xs

pairs :: [a] -> [(a, a)]
pairs c = zip c (rotate 1 c)

triples :: [a] -> [(a, a, a)]
triples c = zip3 (rotate (-1) c) c (rotate 1 c)

edges :: Poly -> [Edge]
edges = map (uncurry Edge) . pairs . points

toLocal :: Vec -> Vec -> Vec
toLocal direction v =  Vec (perp direction `dot` v) (direction `dot` v)

fromLocal :: Vec -> Vec -> Vec
fromLocal direction (Vec h p) = (perp direction) `scale` h + direction `scale` p

polyToLocal :: Vec -> Poly -> Poly
polyToLocal direction p = Poly (fmap (toLocal $ normalize direction) (points p))

polyFromLocal :: Vec -> Poly -> Poly
polyFromLocal direction p = Poly (fmap (fromLocal $ normalize direction) (points p))

collisions :: Poly -> Poly -> [Vec]
collisions a b = catMaybes [ e1 `intersect` e2 | e1 <- edges a, e2 <- edges b ]

simplifyColinear :: (Vec, Vec, Vec) -> Maybe Vec
simplifyColinear (a, b, c) = if abs d > 0.999 then Nothing else Just b
  where d = normalize (a - b) `dot` normalize (c - b)

simplifyClose :: (Vec, Vec) -> Maybe Vec
simplifyClose (a, b) = if a `distance` b < 0.0001 then Nothing else Just a

simplifyStage :: Poly -> Poly
simplifyStage (Poly ps) = Poly . mapMaybe simplifyClose . pairs . mapMaybe simplifyColinear . triples $ ps

simplify :: Poly -> Poly
simplify (Poly ps) = if startLength == endLength then
                       Poly ps
                     else
                       simplify (Poly oneStage)
  where startLength = length ps
        (Poly oneStage) = simplifyStage (Poly ps)
        endLength = length oneStage
