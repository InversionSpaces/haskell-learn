module Functions where

nonExhaustive :: Int -> Int 
nonExhaustive 1 = 2
nonExhaustive 2 = 1

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

firstLetter :: String -> String
firstLetter [] = "empty"
firstLetter str@(l:_) = str ++ "[0]=" ++ [l]

bmiTell :: Double -> Double -> String
bmiTell w h
    | w / h^2 > 30 = "obese"
    | w / h^2 < 18 = "underweight"
    | otherwise = "ok"

bmiTell' :: Double -> Double -> String
bmiTell' w h
    | bmi > 30 = "obese"
    | bmi < 18 = "underweight"
    | otherwise = "ok"
    where bmi = w / h^2

cylinder :: Double -> Double -> Double 
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = 2 * pi * r^2
    in sideArea + 2 * topArea

bigBmis xs = [bmi | (w, h) <- xs, let bmi = w / h^2, bmi > 28]