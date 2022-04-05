module HigherOrderFunctions where

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = let g x y = f y x in g

collatzChain :: Int -> [Int]
collatzChain 1 = [1]
collatzChain n 
    | even n = n : collatzChain (n `div` 2)
    | odd n = n : collatzChain (3 * n + 1)

numLongerChains :: Int -> [Int] -> Int 
numLongerChains longer for = 
    length (filter isLong (map collatzChain for))
    where isLong chain = length chain > longer

numLongerChains' :: Int -> [Int] -> Int 
numLongerChains' longer for = 
    length (filter 
        (\chain -> length chain > longer) 
        (map collatzChain for)
    )