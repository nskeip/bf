module BrainFuck where

data B = B {
    cells :: [Int],
    current :: Int, --TODO: zero based?
    size :: Int --TODO: do i need this? - it aint wildlife!
} deriving (Show)

fillzeros :: Int -> [Int]
fillzeros n = take n $ repeat 0

createB :: Int -> B
createB n = B (fillzeros(n)) 1 n

cell_op :: (Int -> Int) -> Int -> [Int] -> [Int]
cell_op f n (xs) = (take n xs) ++ [f $ xs !! n] ++ (drop (n + 1) xs)

inc :: B -> B
inc b = B (cell_op (+1) (-1 + current b) (cells b)) (current b) (size b)

dec :: B -> B
dec b = B (cell_op (+(-1)) (-1 + current b) (cells b)) (current b) (size b)

fwd :: B -> B
fwd b
    | current b < size b = B (cells b) (1 + current b) (size b)
    | otherwise          = B (cells b) 1 (size b)


back :: B -> B
back b
    | current b > 1 = B (cells b) (-1 + current b) (size b)
    | otherwise     = B (cells b) (size b) (size b)

