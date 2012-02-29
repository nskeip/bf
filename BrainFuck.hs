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

main :: IO ()
main = runStateT code (createB 5) >> return ()

get_curr :: B -> Int
get_curr b = (cells b) !! (pos b)



apply :: (Int -> Int) -> StateT B IO ()
apply f = do
            b <- get
            put $ let n = pos b
                      h = take n $ cells b -- head
                      t = drop (n + 1) $ cells b -- tail
                      val = f $ get_curr b
                      in B n $ h ++ [val] ++ t
            return ()

fwd_helper :: B -> B
fwd_helper b
    | pos b < -1 + size b = B (1 + pos b) (cells b)
    | otherwise           = B 0 (cells b)

fwd :: StateT B IO ()
fwd = do
        b <- get
        put $ fwd_helper b
        return ()

back_helper :: B -> B
back_helper b
        | pos b > 0 = B (-1 + pos b) (cells b)
        | otherwise = B (-1 + size b) (cells b)

back :: StateT B IO ()
back = do
        b <- get
        put $ back_helper b
        return ()

output :: StateT B IO ()
output = do
    b <- get
    liftIO $ print $ get_curr b
    liftIO $ print $ show b
    return ()

code :: StateT B IO ()
code = do
    apply (+10)
    apply (+20)
    output

    back
    apply (+1)
    output
    
    fwd
    apply (+2)
    output

    return ()