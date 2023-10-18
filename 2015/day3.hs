module Main where
import Data.List (nub)
import Data.Bifunctor (bimap)

type Point = (Int, Int)

convert :: Char -> Point
convert c = case c of
  '^' -> (1, 0)
  'v' -> (-1, 0)
  '>' -> (0, 1)
  '<' -> (0, -1)
  _   -> (0, 0)

calc :: [Point] -> [Point]
calc = calcAcc (0,0)
  where
    calcAcc :: Point -> [Point] -> [Point]
    calcAcc acc [] = [(0,0)]
    calcAcc (x, y) (p:ps) = Data.Bifunctor.bimap (x +) (y +) p : calcAcc (Data.Bifunctor.bimap (x +) (y +) p) ps

numUnique :: Eq a => [a] -> Int
numUnique = length . nub

main = do
  xs <- readFile "./day3.txt"

  let points = map convert xs
  let santaPoints = map fst $ filter (even . snd) $ zip points [0..]
  let robotPoints = map fst $ filter (odd . snd) $ zip points [0..]

  let route = calc points
  let santaRoute = calc santaPoints
  let robotRoute = calc robotPoints

  let houses = numUnique route
  let combinedRoute = santaRoute ++ robotRoute
  let visitedHouses = numUnique combinedRoute

  print $ "part 1: " ++  show houses ++ " part 2: " ++ show visitedHouses
