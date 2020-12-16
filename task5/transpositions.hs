swapElementsHelper :: [a] -> Int -> Int -> [a]
swapElementsHelper xs i j = let 
                            elemI = xs !! i
                            elemJ = xs !! j
                            firstPart = take i xs
                            secondPart = take (j - i - 1)  (drop (i + 1) xs)
                            thirdPart = drop (j + 1) xs
                            in firstPart ++ [elemJ] ++ secondPart ++ [elemI] ++ thirdPart                

swapElements :: [a] -> Int -> Int -> [a]
swapElements xs i j
    | i == j    = xs
    | i < j     = swapElementsHelper xs i j
    | i > j     = swapElementsHelper xs j i

transpositionsHelper :: [a] -> Int -> Int -> [[a]]
transpositionsHelper xs l r
    | l == r = [xs]
    | otherwise = concat (map (\i -> transpositionsHelper (swapElements xs l i) (l + 1) r) [l..r])

transpositions :: [a] -> [[a]]
transpositions xs = transpositionsHelper xs 0 $(length xs) - 1