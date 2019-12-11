module Day2 (run) where

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

run :: [Integer] -> [Integer]
run f = f
