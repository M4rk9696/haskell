import FirstSet

-- Problem 11
data Dup a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) =>  [a] -> [Dup a]
encodeModified x = map conv $ encode x
  where 
    conv (1, x) = Single x
    conv (n, x) = Multiple n x

-- Problem 12
decodeModified :: (Eq a) => [Dup a] -> [a]
decodeModified lst = concatMap decode lst
  where
    decode (Single x) = [x]
    decode (Multiple n x) = (take n) . repeat  $ x

-- Problem 13

-- Problem 14
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = [x, x] ++ dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _     = []
repli (x:xs) n = ((take n . repeat) x) ++ repli xs n

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery x n  = (take (n-1) x) ++ (dropEvery (drop n x) n)

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split x n = (take n x, drop n x)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice x a b = (take b) . (drop a) $ x

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate x n = (drop n x) ++ (take n x)

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n x = (x!!n, (take (n-1) x) ++(drop n x))