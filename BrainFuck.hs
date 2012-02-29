module BrainFuck where

import Control.Monad.State

data B = B { 
    pos :: Int, 
    cells :: [Int] 
} deriving (Show)

createB :: Int -> B
createB n = B 0 (fillzeros(n))
                where fillzeros n = take n $ repeat 0

size :: B -> Int
size b = length $ cells b

get_curr :: B -> Int
get_curr b = (cells b) !! (pos b)

apply :: (Int -> Int) -> B -> B
apply f b = let n = pos b
                h = take n $ cells b       -- head
                t = drop (n + 1) $ cells b -- tail
                val = f $ get_curr b
                in B n $ h ++ [val] ++ t

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
    liftIO $ print $ get_curr b
    return ()

eval :: [Char] -> StateT B IO ()
eval [] = return ()
eval (x:xs) = do
                b <- get

                put $ case x of
                        '+' -> apply (+1) b
                        '-' -> apply (-1) b
                        '>' -> fwd b
                        '<' -> back b
                        _   -> b

                case x of
                    '.' -> output

                eval xs

main :: IO ()
main = runStateT (eval "++") (createB 5) >> return ()
