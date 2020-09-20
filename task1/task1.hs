-- map func
mapX :: (a -> b) -> [a] -> [b]  
mapX _ [] = []  
mapX f (x:xs) = f x : mapX f xs

-- flatten
flatten :: [[a]] -> [a]
flatten [] = []
flatten ([]:ys) = flatten ys
flatten ((x:xs):ys) = x:flatten (xs:ys)

-- factorial
factorialHelper :: Int -> Int -> Integer
factorialHelper n a
    | n == 0    = toInteger a
    | otherwise = factorialHelper (n - 1) (n * a)

factorial :: Int -> Integer
factorial n = factorialHelper n 1

-- sin
sinHelper :: Int -> Int -> Double -> Double -> Double -> Double
sinHelper n stepn x stepx current 
    | n + 1 == stepn = current
    | otherwise      =  sinHelper n (stepn + 1) x ((stepx * (-1.0) * x * x) / (fromIntegral ((2 * (stepn + 1)) * (2 * (stepn + 1) + 1)) :: Double)) (stepx + current)

sinX :: Double -> Int -> Double
sinX x n = sinHelper n 1 x ( (x * x  * x) /(-6.0)) x