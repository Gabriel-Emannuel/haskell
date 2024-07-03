data BST a = Empty | Node a (BST a) (BST a) deriving (Show, Eq)

insert :: (Ord a) => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node y leftNode rightNode) 
    | x == y = Node y leftNode rightNode
    | x < y  = Node y (insert x leftNode) rightNode 
    | otherwise  = Node y leftNode (insert x rightNode)

isIn :: (Ord a) => a -> BST a -> Bool
isIn x bst = searchNode x bst /= Empty

searchNode :: (Ord a) => a -> BST a -> BST a
searchNode _ Empty = Empty
searchNode a (Node b leftNode rightNode) 
    | a == b = Node b leftNode rightNode
    | a < b  = searchNode a leftNode
    | otherwise = searchNode a rightNode

minView :: (Ord a) => BST a -> [a]
minView Empty = []
minView (Node a leftNode rightNode) = minView leftNode ++ [a] ++ minView rightNode

maxView :: (Ord a) => BST a -> [a]
maxView Empty = []
maxView (Node a leftNode rightNode) = maxView rightNode ++ [a] ++ maxView leftNode

minimumBST :: (Ord a) => BST a -> a
minimumBST Empty = error "BST doesn't have elements"
minimumBST (Node x Empty rightNode) = x 
minimumBST (Node x leftNode _) = minimumBST leftNode

maximumBST :: (Ord a) => BST a -> a
maximumBST Empty = error "BST doesn't have elements"
maximumBST (Node x leftNode Empty) = x
maximumBST (Node x _ rightNode) = maximumBST rightNode

deleteNode :: (Ord a) => BST a -> BST a
deleteNode Empty = Empty
deleteNode (Node x Empty Empty) = Empty
deleteNode (Node x leftNode Empty) = leftNode
deleteNode (Node x Empty rightNode) = rightNode
deleteNode (Node x leftNode rightNode) = Node (minimumBST rightNode) leftNode (deleteMinimum rightNode)

deleteMinimum :: (Ord a) => BST a -> BST a
deleteMinimum Empty = Empty
deleteMinimum (Node x Empty Empty) = Empty
deleteMinimum (Node x Empty rightNode) = rightNode
deleteMinimum (Node x leftNode rightNode) = Node x (deleteMinimum leftNode) rightNode