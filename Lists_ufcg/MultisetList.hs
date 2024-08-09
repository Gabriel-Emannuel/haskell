module Lists_ufcg.MultisetList () where

import Data.List as List

insertMultiSetList :: (Eq a) =>  a -> [(a, Int)] -> [(a, Int)]
insertMultiSetList elem [] = [ ( elem, 1 ) ]
insertMultiSetList elem ( (x, i) : xs) 
    | x == elem = (x, i + 1) : xs
    | otherwise = (x, i) : insertMultiSetList elem xs

removeMultiSetList :: (Eq a) => a -> [(a, Int)] -> [(a, Int)]
removeMultiSetList elem [] = []
removeMultiSetList elem ((x, i) : xs) 
    | x /= elem = (x, i) : removeMultiSetList elem xs
    | x == elem && i > 1 = (x, i - 1) : xs
    | otherwise = xs

search :: (Eq a) => a -> [(a, Int)] -> (a, Int)
search t [] = (t, 0)
search t ((a, i) : xs) 
    | t == a    = (a, i)
    | otherwise = search t xs

unionMultiSetList :: (Eq a) => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
unionMultiSetList [] bag2 = bag2
unionMultiSetList ((a, i): xs) bag2 
    | x /= 0    = (a, i + x) : (unionMultiSetList xs . filter (\(z, _) -> z /= a)) bag2
    | otherwise = (a, i) : unionMultiSetList xs bag2
    where 
        (t, x) = search a bag2

intersection :: (Eq a) => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
intersection [] bag2 = []
intersection ( ( t, i ) : xs ) bag2 
    | min y i /= 0 = (t, min y i) : intersection xs bag2
    | otherwise    = (t, i) : intersection xs bag2   
        where 
            (a, y) = search t bag2

minus :: (Eq a) => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
minus [] bag2 = []
minus ((t, i) : xs) bag2 
    | y == 0    = (t, i) : minus xs bag2
    | y > i     = minus xs bag2
    | otherwise = (t, i) : minus xs bag2
    where
        (a, y) = search t bag2

inclusion :: (Eq a) => [(a, Int)] -> [(a, Int)] -> Bool
inclusion [] bag2 = True
inclusion ((a, i) : xs) bag2 = y >= i && inclusion xs bag2
    where
        (t, y) = search a bag2

sumMultiSetList :: (Eq a) => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
sumMultiSetList [] bag2 = bag2
sumMultiSetList ((a, i) : xs) bag2 = 
    (a, i + y) : (sumMultiSetList xs . filter (\(z, _) -> z /= a )) bag2
    where
        (_, y) = search a bag2

sizeMultiSetList :: [(a, Int)] -> Int
sizeMultiSetList [] = 0
sizeMultiSetList ((_, i) : xs) = i + sizeMultiSetList xs