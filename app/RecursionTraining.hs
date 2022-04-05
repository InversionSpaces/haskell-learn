module RecursionTraining where

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (p : xs) = 
    let leq = [a | a <- xs, a <= p]
        gt = [a | a <- xs, a > p]
    in quicksort leq ++ [p] ++ quicksort gt