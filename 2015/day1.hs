module Main where

f '(' = 1
f ')' = -1
f _   = 0

solve _ _ [] = 0
solve index current (i:is)
        | current == -1 = index
        | otherwise = solve (index+1) (current + f i) is

main = do
 xs <- readFile "./building_directions.txt"
 let endFloor = sum $ map f xs
 let index = solve 0 0 xs
 print $ show endFloor
 print $ show index
