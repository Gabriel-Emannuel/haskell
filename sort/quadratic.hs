bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = 
    if isOrdered xs
    then xs
    else bubbleSort (bubble xs)

bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs) = 
    if y > x
    then x : bubble (y:xs)
    else y : bubble (x:xs) 

isOrdered :: Ord a => [a] -> Bool
isOrdered [] = True
isOrdered [x] = True
isOrdered (x:y:xs) = y > x && isOrdered (y:xs) 

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = minimumList : (selectionSort.removeElement minimumList) xs
    where
        minimumList = minimum xs

removeElement ::(Eq a) => a -> [a] -> [a]
removeElement y [] = []
removeElement y (x:xs) = if y == x then xs else x : removeElement y xs

insertionSort :: (Ord a) => [a] -> [a]
insertionSort xs = insert xs []

insert :: (Ord a) => [a] -> [a] -> [a]
insert [] xs = xs
insert (x:xs) ys = (insert xs . order) (x : ys)

order :: (Ord a) => [a] -> [a]
order [] = []
order [x] = [x]
order (x:y:xs) = if y > x then x:y:xs else y : order (x:xs)