module BrainFuck where

data B = B { pos :: Int, cells :: [Int] } deriving (Show)

createB :: Int -> B
createB n = B 0 (fillzeros(n))
            where fillzeros n = take n $ repeat 0


b_current_cell_op :: B -> (Int -> Int) -> B
b_current_cell_op b f = B n ((take n xs) ++ [f $ xs !! n] ++ (drop (n + 1) xs))
                        where xs = cells b
                              n  = pos b


inc :: B -> B
inc b = b_current_cell_op b (+1)


dec :: B -> B
dec b = b_current_cell_op b (+(-1))


size :: B -> Int
size b = length $ cells b


fwd :: B -> B
fwd b
    | pos b < size b = B (1 + pos b) (cells b)
    | otherwise      = B 0 (cells b)


back :: B -> B
back b
    | pos b > 0     = B (-1 + pos b) (cells b)
    | otherwise     = B (-1 + size b) (cells b)


eval :: [Char] -> B
eval s = foldl op initial_machine s
            where initial_machine = createB 30
                  op machine op_code = case op_code of
                                          '+' -> inc machine
                                          '-' -> dec machine
                                          '>' -> fwd machine
                                          '<' -> back machine

