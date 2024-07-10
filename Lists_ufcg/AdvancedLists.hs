{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concat" #-}
{-# HLINT ignore "Use foldl1" #-}
{-# HLINT ignore "Use sum" #-}
import Sort.Logarithm (mergeSort, quickSort)

penultimo :: [p] -> p
penultimo [] = error "List doesn't contains elements"
penultimo [x] = error "List doesn't contains elements"
penultimo [x, y] = x
penultimo (x:xs) = penultimo xs


elementAt :: (Ord p) => Int -> [p] -> p
elementAt i xs = mergeSort xs !! i

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs = head xs == last xs && (isPalindrome . init . tail ) xs

compress :: (Eq a) => [a] -> [a]
compress = unique

compact :: Eq a => [a] -> [a]
compact [] = []
compact (x:xs) = x : filter (== x) xs ++ (compact . filter (/= x)) xs

encode :: (Eq a) => [a] -> [(a, Int)]
encode [] = []
encode (x:xs) = (x, 1 + (length . filter ( == x)) xs) : (encode . filter ( /= x)) xs

split :: [p] -> Int -> [[p]]
split xs i = [take i xs, drop i xs]

slice :: [a] -> Int -> Int -> [a]
slice xs imin imax = (take imax . drop (imin - 1)) xs

insertAt :: p -> Int -> [p] -> [p]
insertAt el position xs = take (pred position) xs ++ [el] ++ drop (pred position) xs

minList :: Ord c => [c] -> c
minList [] = error "List doesn't contains elements"
minList [x] = x
minList (x:xs) = (min x . minList) xs

remove :: Eq t => t -> [t] -> [t]
remove _ [] = error "List doesn't contains elements"
remove e (x:xs) | e == x = xs
                | otherwise = x: remove e xs

sort :: Ord a => [a] -> [a]
sort [] = []
sort xs = x:ys
    where
        x = minList xs
        ys = sort (remove x xs)

maxList :: (Ord p) => [p] -> p
maxList [] = error "This List doesn't contains elements"
maxList xs = foldl max (head xs) (tail xs)

buildPalindrome :: [a] -> [a]
buildPalindrome [] = []
buildPalindrome xs = [head xs] ++ (buildPalindrome . tail ) xs ++ [head xs]

mean :: (Num a, Fractional a) => [a] -> a
mean xs = mySum / myLength xs
    where
        mySum = foldr (+) 0 xs
        myLength = \xs -> if null xs then 0 else (myLength . tail) xs

myAppend :: Foldable t => [a] -> t a -> [a]
myAppend = foldr (:)

find :: (p -> Bool) -> [p] -> p
find p xs
    | null predicated = error "error"
    | otherwise = head predicated
    where
        predicated = filter p xs

quickSortFilter :: (Ord a) => [a] -> [a]
quickSortFilter [] = []
quickSortFilter (x:xs) = (quickSortFilter . filter ( < x)) xs ++ [x] ++ (quickSortFilter . filter ( >= x)) xs

sumFoldl :: (Num a) => [a] -> a
sumFoldl = foldl (+) 0

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs) = x : (unique . filter (/= x)) xs

count :: (Eq a) => a -> [a] -> Int
count y = length . filter ( == y)

frequencia :: (Eq a) => [a] -> [(a, Int)]
frequencia [] = []
frequencia xs = (unique . map (\x -> (x, count x xs))) xs

concatFoldr :: [[a]] -> [a]
concatFoldr = foldr (++) []