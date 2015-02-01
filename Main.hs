module Main where

import Data.Maybe
import Data.Monoid

import Graphics.Gloss hiding (Line)
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

reified = shunt (Vec 0 1) square2 shape

levels = scanlevels shape square2

foo :: [Vec]
foo = points reified

main :: IO ()
main = display (InWindow "Shunt" (600, 600) (300, 300)) white (polysPicture [shape, reified, square2] <> dots)
  where dots = mconcat $ fmap dotPicture foo
