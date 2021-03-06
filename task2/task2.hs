primes :: [Int]
primes = sieve [2..]
            where 
            sieve (p:xs) = p : sieve [x | x <- xs, mod x p > 0]

minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case (compare x y) of 
   LT -> x : minus  xs (y:ys)
   EQ ->     minus  xs    ys
   GT ->     minus (x:xs) ys
minus  xs     _     = xs

primesEratos :: [Int]
primesEratos = sieve [2..] 
           where
           sieve (p:xs) = p : sieve (minus xs [p, p + p..])

foldrX :: (a -> b -> b) -> b -> [a] -> b
foldrX f z []     = z 
foldrX f z (x:xs) = f x (foldrX f z xs) 

foldlX :: (a -> b -> a) -> a -> [b] -> a
foldlX f z []     = z                  
foldlX f z (x:xs) = foldlX f (f z x) xs

unfold :: (a -> Maybe (a, b)) -> a -> [b]
unfold f x = maybe [] (\(u, v) -> v : (unfold f u)) (f x)

binaryUnfolder :: Int -> Maybe (Int, Int)
binaryUnfolder 0 = Nothing
binaryUnfolder i = Just (div i 2, mod i 2)

binaryDigits :: Int -> [Int]
binaryDigits 0 = [0]
binaryDigits i = reverse (unfold binaryUnfolder i)