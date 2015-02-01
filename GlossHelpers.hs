module GlossHelpers where

import Data.Monoid

import GHC.Float
import Graphics.Gloss

import Vec
import Poly

toGloss :: Vec -> Point
toGloss (Vec x y) = (double2Float x, double2Float y)

dotPicture :: Vec -> Picture
dotPicture v = translate x y $ color red $ circle 3
  where (x, y) = toGloss v

polyPicture :: Color -> Poly -> Picture
polyPicture _ (Poly []) = mempty
polyPicture c (Poly p) = pictures [color c $ polygon glossPoints,
                                   color black $ line outline]
  where glossPoints = map toGloss p
        outline = glossPoints ++ [head glossPoints]

mod1 :: Float -> Float
mod1 f = d where (_, d) = properFraction f

hsv :: (Float,Float,Float) -> Color
hsv (h, s, v) = case hi of
    0 -> makeColor v t p 1
    1 -> makeColor q v p 1
    2 -> makeColor p v t 1
    3 -> makeColor p q v 1
    4 -> makeColor t p v 1
    5 -> makeColor v p q 1
 where
  hi = floor (h/60) `mod` 6
  f = mod1 (h/60)
  p = v*(1-s)
  q = v*(1-f*s)
  t = v*(1-(1-f)*s)

distinctColours :: [Color]
distinctColours = map (\i -> hsv (hue i, 0.5, 0.95)) [1..]
  where phi = 0.6180339887
        hue i = 360 * mod1 (i * phi)

polysPicture :: [Poly] -> Picture
polysPicture polys = pictures $ zipWith polyPicture distinctColours polys
