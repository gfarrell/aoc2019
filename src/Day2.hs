module Day2 (run, has, validate, splice) where

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
validate (99:xs) = True
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
    in iter (i + 4) $ splice (op a b) out $ mem
  | otherwise = mem

