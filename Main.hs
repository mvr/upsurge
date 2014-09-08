module Main where

import Data.Maybe
import Data.Monoid

import Graphics.Gloss hiding (Line)
import GlossHelpers

import Vec
import Poly

square :: Poly
square = Poly [Vec 0 0, Vec 50 0, Vec 100 100, Vec 0 100]

trans :: Vec -> Poly -> Poly
trans v (Poly ps) = Poly $ fmap (+v) ps

square2 :: Poly
square2 = trans (Vec (-25) 25) square

intersectingLine :: Line
intersectingLine = Line (Vec (-50) 0) (Vec 1 1.5)

foo :: [Vec]
foo = collisionPoints $ fromJust $ collidePoly square square2

-- test :: [Poly]
-- test = [a, b]
--   where (a, b) = slice square (BP 0 0.5) (BP 2 0.5)

main :: IO ()
main = display (InWindow "Shunt" (600, 600) (300, 300)) white (polysPicture [square, square2] <> dots)
  where dots = mconcat $ fmap dotPicture foo
