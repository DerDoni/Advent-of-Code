module Main where

import Data.String.Utils

type Dimension = (Int, Int, Int)
convert :: [String] -> Dimension
convert [l, w, h] = (read l, read w, read h)


volume :: Dimension -> Int
volume (l, w, h) = l * w * h

shortestRibbon :: Dimension -> Int
shortestRibbon (l, w, h)
  | (l < w && w < h) || (w < l && l < h ) = w + w + l + l
  | (l < h && h < w) || (h < l && l < w ) = l + l + h + h
  | (w < h && h < l) || (h < w && w < l ) = w + w + h + h
  | otherwise = 2 * (l + w + h - maximum [l, w, h])
calculate :: Dimension -> Int
calculate (l, w, h) = 2*l*w + 2*w*h + 2*h*l + extra (l, w, h)
  where
    extra (l, w, h) = min (l*w) (min (w*h)  (h*l))

main =
  do
    presents <- lines <$> readFile "./presents.txt"
    let dims = map (convert. split "x") presents
    let result = sum $ map calculate dims
    let volumes = sum $ map volume dims
    let shortest = sum $ map shortestRibbon dims
    return $ shortest + volumes
