module BrainFuck where

import Data.Char

data B = B { pos :: Int, cells :: [Int] } deriving (Show)

size :: B -> Int
size b = length $ cells b

fwd :: B -> IO B
fwd b
    | pos b < size b = return $ B (1 + pos b) (cells b)
    | otherwise      = return $ B 0 (cells b)

back :: B -> IO B
back b
    | pos b > 0 = return $ B (-1 + pos b) (cells b)
    | otherwise = return $ B (-1 + size b) (cells b)

get_curr :: B -> Int
get_curr b = (cells b) !! (pos b)

set_curr :: B -> Int -> B
set_curr b val = B n ((take n xs) ++ [val] ++ (drop (n + 1) xs))
                    where xs = cells b
                          n  = pos b

inc :: B -> IO B
inc b = return $ set_curr b new_val
            where new_val = 1 + (get_curr b)

dec :: B -> IO B
dec b = return $ set_curr b new_val
            where new_val = -1 + (get_curr b)

output :: B -> IO B
output b = (putStr [(chr $ (cells b) !! (pos b))]) >> return b

input :: B -> IO B
input b = (getLine >>= return . ord . head) >>= return . set_curr b

noop :: B -> IO B
noop b = return b

command :: Char -> (B -> IO B)
command '+' = inc
command '-' = dec
command '>' = fwd
command '<' = back
command '.' = output
command ',' = input
command _   = noop

createB :: Int -> B
createB n = B 0 (fillzeros(n))
                where fillzeros n = take n $ repeat 0

eval_helper :: Int -> [Char] -> B -> IO B
eval_helper n ops b = if (n + 1) > length(ops)
                        then 
                            return b
                        else
                            current_op b >>= eval_helper (n + 1) ops
                                   where current_op = command $ ops !! n

eval :: [Char] -> IO B
eval [] = return $ createB 30
eval s  = eval_helper 0 s (createB 30)


