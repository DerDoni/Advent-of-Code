module Main where
import Data.Char (isDigit)

solve :: [String] -> Integer
solve puzzle =  sum $ map (read .(\x -> head x : [last x])) puzzle

numbers :: [String] -> [String]
numbers = map . filter $ isDigit

main = do
  calibration_values <- lines <$> readFile "./1.txt"
  return $ solve $ numbers calibration_values
