module FirstSet (pack, encode) where

-- Problem 1
myLast :: [a] -> a
myLast [x]  = x
myLast (_:xs) = myLast xs
myLast _   = error "No elem"

-- Problem 2
myButLast :: [a] -> a
myButLast [x, _]  = x
myButLast (_: xs) = myButLast xs
myButLast _       = error "No elem"

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt x n = x !! n

-- Problem 4
myLength :: [a] -> Int
myLength = foldr (\_ -> (+1)) 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse [x]    = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome = myReverse >>= (==)

-- Problem 7
data NestedList a = Elem a | List [NestedList a] deriving(Show)

flatten :: (NestedList a) -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress (x:xs) = x : (compress $ dropWhile (==x) xs)

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x : takeWhile (==x) xs) : (pack $ dropWhile (==x) xs)

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode x = [((myLength n), head n) | n <- pack x]