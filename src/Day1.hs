module Day1
    (
    mass_to_fuel,
    how_much_fuel,
    total_fuel,
    total_ship_fuel,
    lines_to_list,
    part1,
    part2
    ) where

import System.IO
import Data.String

mass_to_fuel :: Integer -> Integer
mass_to_fuel n
           | n < 6     = 0
           | otherwise = (floor (fromIntegral n / 3.0)) - 2

how_much_fuel :: [Integer] -> Integer
how_much_fuel [] = 0
how_much_fuel (x:xs) = (mass_to_fuel x) + (how_much_fuel xs)

total_fuel :: Integer -> Integer
total_fuel n
         | n < 6 = 0
         | otherwise = (mass_to_fuel n) + total_fuel (mass_to_fuel n)

total_ship_fuel :: [Integer] -> Integer
total_ship_fuel [] = 0
total_ship_fuel (x:xs) = (total_fuel x) + (total_ship_fuel xs)

lines_to_list :: String -> [Integer]
lines_to_list t = map readInt . lines $ t

readInt :: String -> Integer
readInt = read

part1 :: String -> IO ()
part1 f = do
  ll  <- readFile f
  print $ "Day 1 / part 1: " ++ (show . how_much_fuel . lines_to_list $ ll)

part2 :: String -> IO ()
part2 f = do
  ll  <- readFile f
  print $ "Day 1 / part 2: " ++ (show . total_ship_fuel . lines_to_list $ ll)
