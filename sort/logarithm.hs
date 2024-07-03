mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge ((mergeSort . take halfLength) xs) ((mergeSort . drop halfLength) xs) 
    where
        halfLength = length xs `div` 2

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) =
    if x > y
    then y : merge (x:xs) ys
    else x : merge xs (y:ys)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort . filter ( < x)) xs ++ [x] ++ (quickSort . filter ( >= x)) xs 