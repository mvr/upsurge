module Poly where

import Control.Lens hiding (levels)
import Data.Maybe
import Data.List (sort, sortBy, nub)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Function

import Debug.Trace

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

data PointType = ScissorLeft | ScissorRight | Normal deriving (Eq, Show)
data ScanlinePoint = ScanlinePoint { scanlinePosition :: Double,
                                     pointType :: PointType,
                                     polyOrder :: Double } deriving (Show)
data Interval = Interval { intervalStart :: Double,
                           intervalEnd :: Double,
                           intervalPoints :: [ScanlinePoint] } deriving (Show)
-- Should possibly have left and right
type ScanlineRep = Map Double [Interval]

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

scanlevels :: Poly -> Poly -> [Double]
scanlevels a b = sort $ nub $ map _x (points a ++ points b ++ collisions a b)

pointAtLevel :: Double -> Edge -> Vec
pointAtLevel level (Edge (Vec x1 y1) end@(Vec x2 y2)) = if x1 == x2 then end else Vec level newpos
  where newpos =  ((y2 - y1) / (x2 - x1)) * (level - x1) + y1

scanEdge :: [Double] -> Edge -> [Vec]
scanEdge levels e@(Edge start end) = if start^.x > end^.x then
                                       reverse $ scanEdge levels (Edge end start)
                                     else
                                       fmap (\l -> pointAtLevel l e) between
  where between = filter (\h -> start^.x <= h && h <= end^.x) levels

-- this is not nub, removes consecutive duplicates, treating the list as a loop
-- todo refactor
removedups :: Eq a => [a] -> [a]
removedups [] = []
removedups (a:as) = let us = go (a:as ++ [a]) in
                     take (length us - 1) us
  where go (a:b:as) | a == b    = go (a:as)
                    | otherwise = a : go (b:as)
        go (a:as)               = a : go as
        go []                   = []

scanPoly :: [Double] -> Poly -> [Vec]
scanPoly levels p = removedups $ concatMap (scanEdge levels) (edges p)

constructScanrep :: [Double] -> Poly -> ScanlineRep
constructScanrep levels p = fmap findIntervals $ M.fromListWith (++) assoc
  where ps = scanPoly levels p
        dotriple (triple, i) = (h, [ScanlinePoint pos (classifyTriple triple) i])
          where (_, Vec h pos, _) = triple
        assoc = fmap dotriple $ zip (triples ps) [1..]

classifyTriple :: (Vec, Vec, Vec) -> PointType
classifyTriple (l, c, r) | l^.x < c^.x && r^.x < c^.x = ScissorLeft
                         | l^.x > c^.x && r^.x > c^.x = ScissorRight
                         | otherwise                  = Normal

findIntervals :: [ScanlinePoint] -> [Interval]
findIntervals s = go [] sorted
  where -- Scissor intervals
        go [] (p@(ScanlinePoint pos ScissorLeft _):rest)  = (Interval pos pos [p]) : go [] rest
        go [] (p@(ScanlinePoint pos ScissorRight _):rest) = (Interval pos pos [p]) : go [] rest

        -- Start normal interval
        go [] (p@(ScanlinePoint _ Normal _):rest)         = go [p] rest

        -- End normal interval
        go as (p@(ScanlinePoint end Normal _):rest)       = (Interval start end (as ++ [p]) ) : go [] rest
          where start                                     = scanlinePosition (head as)

        -- Continue normal interval
        go as (p@(ScanlinePoint _ _ _):rest)              = go (as ++ [p]) rest

        go _ [] = []

        sorted = sortBy (compare `on` scanlinePosition) s

intervalLength :: Interval -> Double
intervalLength i = intervalEnd i - intervalStart i

shiftInterval :: Interval -> Double -> Interval
shiftInterval Interval{ intervalStart = s, intervalEnd = e, intervalPoints = ps} amount = Interval (s + amount) (e + amount) (map shiftPoint ps)
  where shiftPoint p@ScanlinePoint{ scanlinePosition = pos } = p { scanlinePosition = pos + amount }

shuntLine :: [Interval] -> [Interval] -> [Interval]
shuntLine = go (read "-Infinity")
  where go _ (f:fs) (m:ms) | intervalStart f < intervalEnd m = go (intervalEnd f) fs (m:ms)
        go b fs     (m:ms) | intervalStart m < b             = shifted : go (intervalEnd shifted) fs ms
          where shifted = shiftInterval m (b - intervalStart m)
        go _ fs     (m:ms)                                   = m : go (intervalEnd m) fs ms
        go _ _ []                                            = []

-- I should just use lens
reifyPoly :: ScanlineRep -> Poly
reifyPoly sr = Poly $ fmap snd $ sortBy (compare `on` fst) ipoints
  where assoc = M.toList $ fmap (concatMap intervalPoints) sr
        ipoints = concatMap (\(h, ps) -> fmap (\p -> (polyOrder p, Vec h (scanlinePosition p))) ps) assoc

simplify :: Poly -> Poly
simplify poly = poly

shunt :: Vec -> Poly -> Poly -> Poly
shunt direction f m = polyFromLocal direction $ reifyPoly cr
  where f' = polyToLocal direction f
        m' = polyToLocal direction m
        ss = scanlevels f' m'
        fr = constructScanrep ss f'
        mr = constructScanrep ss m'
        cr = M.mergeWithKey (\k a b -> Just $ shuntLine a b) (const M.empty) id fr mr
