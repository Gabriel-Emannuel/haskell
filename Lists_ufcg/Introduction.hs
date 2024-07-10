xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True

impl :: Bool -> Bool -> Bool
impl True False = False
impl _ _ = True

equiv :: Bool -> Bool -> Bool
equiv a b = impl a b && impl b a

square :: Num a => a -> a
square x = x*x

pow :: (Num x) => x -> Int -> x
pow x y = ( product.replicate y) x

fatorial :: Int -> Int
fatorial x = product [1..x]

isPrime :: Int -> Bool
isPrime x 
    | x > 2 = null [n | n <- [2 .. x], mdc x n /= 1]
    | otherwise = False

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}

geraListaFib :: [Int]
geraListaFib = 0 : 1 : zipWith (+) geraListaFib (tail geraListaFib)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x 
    | x > 0 = geraListaFib !! (x + 1)
    | otherwise = error "Doesn't exist negative members from fibonnaci"

mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

mmc :: Int -> Int -> Int
mmc x y = x * y `div` mdcCommom
    where
        mdcCommom = mdc x y

coprimo :: Int -> Int -> Bool
coprimo x y = mdc x y == 1 

-- myLength xs = foldr (\x -> (+) 1) 0 xs
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myTake :: (Num a, Ord a) => a -> [t] -> [t]
myTake _ [] = []
myTake k (x:xs)
    | k > 0 = x : myTake (k-1) xs
    | otherwise = []

myDrop :: (Num a, Ord a) => a -> [t] -> [t] 
myDrop _ [] = []
myDrop k (x:xs)
    | k > 0 = myDrop (k-1) xs
    | otherwise = xs

myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "This list contains 0 elements"
myMaximum [x] = x
myMaximum (x:xs) = (max x . myMaximum) xs

myMinimum :: (Ord a) => [a] -> a
myMinimum [] = error "This list contains 0 elements"
myMinimum [x] = x
myMinimum (x:xs) = (min x . myMinimum) xs

-- mySum xs = foldl (+) 0 xs
mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

-- myProduct xs = foldl (*) 1 xs
myProduct :: (Num a) => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

-- myElem a xs = foldl (\ x -> (||) (a == x)) False xs
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = a == x || myElem a xs

myRange :: (Num a, Ord a, Enum a) => a -> a -> [a] 
myRange k m 
    | k > m = []
    | otherwise = k : (myRange . succ) k m

myRangeStep :: (Num a, Ord a) => a -> a -> a -> [a]
myRangeStep k p m 
    | k > m = []
    | otherwise = k : myRangeStep (k+p) p m

myCycle :: [a] -> [a]
myCycle xs = xs ++ myCycle xs

myRepeat :: t -> [t]
myRepeat n = n : myRepeat n

myReplicate :: (Ord a, Num a, Enum a) => a -> t -> [t]
myReplicate k n
    | k > 0 = n : (myReplicate . pred ) k n
    | otherwise = []