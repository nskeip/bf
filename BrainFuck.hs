module Main where

import System
import Data.Char
import Control.Monad.State

data B = B { 
    pos :: Int, 
    cells :: [Int] 
} deriving (Show)

createB :: Int -> B
createB n = B 0 (take n $ repeat 0)

size :: B -> Int
size b = length $ cells b

get_curr :: B -> Int
get_curr b = (cells b) !! (pos b)

apply :: (Int -> Int) -> B -> B
apply f b = let n = pos b
                h = take n $ cells b       -- head
                t = drop (n + 1) $ cells b -- tail
                in B n $ h ++ [f (get_curr b)] ++ t

fwd :: B -> B
fwd b
    | pos b < -1 + size b = B (1 + pos b) (cells b)
    | otherwise           = B 0 (cells b)

back :: B -> B
back b
    | pos b > 0 = B (-1 + pos b) (cells b)
    | otherwise = B (-1 + size b) (cells b)

output :: StateT B IO ()
output = do
    b <- get
    liftIO $ putStr [chr $ get_curr b]
    return ()

input :: StateT B IO ()
input = do
        inp <- liftIO $ getLine
        b <- get
        put $ apply (\_->(ord $ head inp)) b
        return ()

eval_helper :: [Char] -> Int -> StateT B IO ()
eval_helper [] _ = return ()
eval_helper xs i
        | i == length xs = return ()
        | otherwise      = let x = xs !! i
                            in
                            do
                                b <-get

                                put $ case x of
                                        '+'         -> apply (+1) b
                                        '-'         -> apply (subtract 1) b
                                        '>'         -> fwd b
                                        '<'         -> back b
                                        otherwise   -> b

                                case x of
                                    '.'         -> output
                                    ','         -> input
                                    otherwise   -> return ()

                                eval_helper xs (i + 1)

eval xs = eval_helper xs 0

main :: IO ()
main = do
    args <- getArgs
    runStateT (eval $ head args) (createB 30)
    liftIO $ putStr "\n"
    return ()