{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

pow:: Int -> Int -> Int
pow = \x y -> (product . replicate y) x

fatorial:: Int -> Int
fatorial = \x -> product [1..x]

mdc :: Int -> Int -> Int
mdc = \x y -> 
    if y == 0 then x 
    else if x == 0 then y 
    else mdc y (x `mod` y)

mmc :: Int -> Int -> Int 
mmc = \x y -> (x * y) `div` mdc x y

fib :: Int -> Int
fib = \x ->
    if      x == 0 then 0
    else if x == 1 then 1
    else fib (x-1) + fib (x-2)

isPrime :: Int -> Bool
isPrime = \x -> (not . any (\y -> x `mod` y == 0)) [2..(x-1)]

-- Lists

myLast :: [a] -> a
myLast = \xs ->
    if      null xs then error "This List doesn't contains elements"
    else if length xs == 1 then head xs
    else    (myLast . tail) xs

mySecondLast :: [a] -> a
mySecondLast = \xs ->
    if length xs < 2 then error "This List doesn't contain a second last"
    else if length xs == 2 then head xs
    else (mySecondLast . tail) xs

elementAt :: Int -> [a] -> a
elementAt = \i xs ->
    if null xs || i < 0 then error ""
    else if i == 0 then head xs
    else elementAt (pred i) (tail xs)

myLength :: (Fractional a, Num a) => [b] -> a
myLength = \xs ->
    if null xs then 0
    else 1 + (myLength . tail) xs

myReverse :: [a] -> [a]
myReverse = \xs ->
    if null xs then []
    else last xs : (myReverse . init) xs

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome = \xs -> (null xs || head xs == last xs) && (isPalindrome . init. tail) xs

compress :: (Eq a) => [a] -> [a]
compress = \xs ->
    if null xs then []
    else head xs : (compress . filter ( /= head xs)) xs

compact :: (Eq a) => [a] -> [a]
compact = \xs ->
    if null xs then []
    else filter ( == head xs) xs ++ (compact . filter (/= head xs)) xs

encode :: (Eq a) => [a] -> [(a, Int)]
encode = \xs ->
    if null xs then []
    else (head xs, (length . filter ( == head xs)) xs) : (encode . filter (/= head xs)) xs

split :: [a] -> Int -> [[a]]
split = \xs i -> [take i xs, drop i xs] 

slice :: [a] -> Int -> Int -> [a]
slice = \xs imin imax -> (take imax . drop (imin - 1)) xs

insertAt :: a -> Int -> [a] -> [a]
insertAt = \el pos xs -> take (pred pos) xs ++ [el] ++ drop (pred pos) xs

sort :: (Ord a, Eq a) => [a] -> [a]
sort = \xs -> 
    if null xs then []
    else filter (== minimum xs) xs ++ (sort . filter (/= minimum xs)) xs

mySum :: (Num a) => [a] -> a
mySum = \xs ->
    if null xs then 0
    else head xs + (mySum . tail) xs

maxList :: (Ord a) => [a] -> a
maxList = \xs -> 
    if null xs then error "This List doesn't contains elements"
    else if length xs == 1 then head xs
    else max (head xs) ((maxList . tail) xs)

buildPalindrome :: [a] -> [a]
buildPalindrome = \xs -> 
    if null xs then []
    else [head xs] ++ (buildPalindrome . tail) xs ++ [head xs]

mean :: (Fractional a, Num a) => [a] -> a
mean = \xs -> mySum xs / myLength xs

myAppend :: [a] -> [a] -> [a]
myAppend = \xs ys -> xs ++ ys