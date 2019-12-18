module Day2 (
    run
  , has
  , validate
  , with_inputs
  , test_inputs
  , seek
  , part1
  , part2
) where

import Util

-- Intcode programme is a list of integers
-- Each subroutine consists of opcode, then args
-- Opcodes:
--   1 = add
--   2 = multiply
--  99 = terminate
-- For [1, 2] three args, all are indices.
-- Args 1, 2 are input locations for the function.
-- Arg 3 is the output location for the function.
-- After processing a subroutine, move pointer by +4.

run :: [Integer] -> Maybe [Integer]
run mem
  | validate mem == False = Nothing
  | otherwise = Just $ iter 0 mem

validate :: [Integer] -> Bool
-- TODO: the one validation this does not handle is index out of bounds for args
validate (99:_) = True
validate (x:xs)
  | (has x [1, 2]) && (length xs > 3) = validate . drop 3 $ xs
  | otherwise                         = False

iter :: Int -> [Integer] -> [Integer]
iter i mem
  | (mem !! i) == 99 = mem
  | has (mem !! i) [1, 2] == True =
    let op  = case (mem !! i) of { (1) -> (+); (2) -> (*); }
        a   = mem !! toInt (mem !! (i + 1))
        b   = mem !! toInt (mem !! (i + 2))
        out = toInt (mem !! (i + 3))
    in iter (i + 4) . splice (op a b) out $ mem
  | otherwise = mem

with_inputs :: (Integer, Integer) -> [Integer] -> [Integer]
with_inputs (noun, verb) mem = splice noun 1 . splice verb 2 $ mem

test_inputs :: (Integer, Integer) -> [Integer] -> Integer
test_inputs inps mem = head . iter 0 . with_inputs inps $ mem

-- Find the combination of inputs which produces the desired output.
-- Inputs in range (0, 99).
-- First thought would be binary search, but that's not going to work as
-- the "inputs" are actually memory addresses not simply parameters in
-- some sort of polynomial (even though the only instructions, addition
-- and multiplication, are monotonic, increasing functions). Therefore,
-- the only way to do this is to run through all the possibilities I
-- think, which is a bit annoying since it's 100x100 which is 10'000
-- combinations. I suspect, however, that even for long programmes this
-- won't take too long.

seek :: Integer -> [Integer] -> Maybe (Integer, Integer)
seek t mem = _seek_from (0, 0) t mem

_seek_from :: (Integer, Integer) -> Integer -> [Integer] -> Maybe (Integer, Integer)
_seek_from (a, b) t [] = Nothing
_seek_from (a, b) t mem
  | a > bound && b > bound = Nothing
  | a < 0 || b < 0 = Nothing
  | a > bound && b <= bound = _seek_from (0, b + 1) t mem
  | otherwise =
    let r = test_inputs (a, b) mem in
    if r == t then Just (a, b) else _seek_from (a + 1, b) t mem
    where bound = let x = toInteger . (flip (-) 1) . length $ mem in if x > 99 then 99 else x

part1 :: String -> IO ()
part1 f = do
  xs <- readFile f
  print $ "Day 2 / part 1: " ++ (show . run . with_inputs (12, 2) . map readInt . split (==',') $ xs)

part2 :: String -> IO ()
part2 f = do
  xs <- readFile f
  print $ "Day 2 / part 1: " ++ (show . part2' . seek 19690720 . map readInt . split (==',') $ xs)
  where part2' x = case x of { Nothing -> (-1, -1); Just x  -> x }
