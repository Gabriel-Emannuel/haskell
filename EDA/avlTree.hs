data AvlTree a = Empty | Node a (AvlTree a) (AvlTree a) deriving (Show, Eq)

insert :: (Ord a) => a -> AvlTree a -> AvlTree a
insert _ _ = undefined

isIn :: (Ord a) => a -> AvlTree a -> Bool
isIn x avl = searchNode x avl /= Empty

searchNode :: (Ord a) => a -> AvlTree a-> AvlTree a
searchNode x Empty = Empty
searchNode x (Node a leftNode rightNode) 
    | x == a    = Node a leftNode rightNode
    | x < a     = searchNode x leftNode
    | otherwise = searchNode x rightNode

minView :: (Ord a) => AvlTree a -> [a]
minView Empty = []
minView (Node x leftNode rightNode) = minView leftNode ++ [x] ++ minView rightNode

maxView :: (Ord a) => AvlTree a -> [a]
maxView Empty = []
maxView (Node x leftNode rightNode) = maxView rightNode ++ [x] ++ maxView leftNode

deleteNode :: (Ord a) => AvlTree a -> AvlTree a
deleteNode _ = undefined

calculateHeight :: AvlTree a -> Int
calculateHeight Empty = 0
calculateHeight (Node a leftNode rightNode) = 1 + max (calculateHeight leftNode) (calculateHeight rightNode)

calculateBalance :: AvlTree a -> Int
calculateBalance Empty = 0
calculateBalance (Node _ leftNode rightNode) = calculateHeight rightNode - calculateHeight leftNode