module BrainFuck where

data B = B { pos :: Int, cells :: [Int] } deriving (Show)

createB :: Int -> IO B
createB n = return $ B 0 (fillzeros(n))
                where fillzeros n = take n $ repeat 0


b_current_cell_op :: B -> (Int -> Int) -> B
b_current_cell_op b f = B n ((take n xs) ++ [f $ xs !! n] ++ (drop (n + 1) xs))
                        where xs = cells b
                              n  = pos b


inc :: B -> IO B
inc b = return $ b_current_cell_op b (+1)


dec :: B -> IO B
dec b = return $ b_current_cell_op b (+(-1))


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


output :: B -> IO B
output b = do { print $ (cells b) !! (pos b); return b }

eval :: [Char] -> IO B
eval s = foldl (\machine opcode->machine >>= (get_op opcode)) initial_machine s
            where initial_machine = createB 30
                  get_op opcode = case opcode of
                                      '+' -> inc
                                      '-' -> dec
                                      '>' -> fwd
                                      '<' -> back
                                      '.' -> output
