module Util (has, splice, split, lines_to_list, toInt, readInt) where

has :: Eq a0 => a0 -> [a0] -> Bool
has x [] = False
has x (y:ys)
  | x == y = True
  | otherwise = has x ys

splice :: a0 -> Int -> [a0] -> [a0]
splice x i xs = (take i xs) ++ [x] ++ (drop (i + 1) xs)

split :: (Char -> Bool) -> String -> [String]
split pred "" = []
split pred s  = let (a, s') = break pred s
                in a : case s' of
                  []      -> []
                  (_:s'') -> split pred s''

lines_to_list :: String -> [Integer]
lines_to_list t = map readInt . lines $ t

toInt :: Integer -> Int
toInt = fromIntegral

readInt :: String -> Integer
readInt = read
