module Main where

import Data.Maybe
import Data.Monoid

import Graphics.Gloss hiding (Line)
import Graphics.Gloss.Interface.Pure.Game
import GlossHelpers

import Vec
import Poly

shape :: Poly
shape = Poly [Vec 0 0, Vec 50 0, Vec 100 100, Vec 0 100]

trans :: Vec -> Poly -> Poly
trans v (Poly ps) = Poly $ fmap (+v) ps

square :: Poly
square = Poly [Vec 0 0, Vec 50 0, Vec 50 50, Vec 0 50]

square2 :: Poly
square2 = trans (Vec (-20) (-20)) square

reified = shunt (Vec 1 0.7) square2 shape

levels = scanlevels shape square2

foo :: [Vec]
foo = points reified

oldmain :: IO ()
oldmain = display (InWindow "Shunt" (600, 600) (300, 300)) white (polysPicture [shape, reified] <> dots)
  where dots = mconcat $ fmap dotPicture foo

main :: IO ()
main = play (InWindow "Shunt" (600, 600) (300, 300)) white 25 (Vec 1000 1000, shape) draw event (\_ w -> w)
  where draw (p, m) = let dots = mconcat $ fmap dotPicture (points m) in
                       polysPicture [trans p square, m] <> dots
        event (EventMotion p) (old, m) = (p', shunt (p' - old) (trans p' square) m)
          where p' = fromGloss p
        event _ w = w
