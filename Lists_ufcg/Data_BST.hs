module BST (
    BinaryTree(..), 
    isNil,
    getNode,
    sizeBST,
    isBST,
    insert,
    search,
    maximumBST,
    minimumBST,
    predecessor,
    successor,
    removeBST,
    removeNode,
    order,
    preOrder,
    postOrder
) where

data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

isNil :: BinaryTree a -> Bool
isNil NIL = True
isNil _ = False

getNode :: BinaryTree a -> a
getNode NIL = error "Node doesn't exist"
getNode (Node a _ _) = a

sizeBST :: BinaryTree a -> Int
sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

isBST :: (Ord a) => BinaryTree a -> Bool
isBST NIL = True
isBST (Node n left right) 
    | not (isNil left)  && getNode left  > n = False
    | not (isNil right) && getNode right < n = False
    | otherwise = True

insert :: (Ord a) => BinaryTree a -> a -> BinaryTree a
insert NIL a = Node a NIL NIL
insert (Node t left right) a 
    | a > t = Node t left (insert right a)
    | a < t = Node t (insert left t) right
    | otherwise = Node t left right

search :: (Ord a) => BinaryTree a -> a -> BinaryTree a
search NIL _ = NIL
search (Node t left right) a 
    | a > t = search right a
    | a < t = search left a
    | otherwise = Node t left right

maximumBST :: (Ord a) => BinaryTree a -> a
maximumBST NIL = error "Node doesn't contains elements"
maximumBST (Node a _ NIL) = a
maximumBST (Node a _ right) = maximumBST right

minimumBST :: (Ord a) => BinaryTree a -> a
minimumBST NIL = error "Node doens't contains elements"
minimumBST (Node a NIL _) = a
minimumBST (Node a left _) = minimumBST left

predecessor :: (Ord a) => BinaryTree a -> a
predecessor NIL = error "Node doesn't contains predecessor"
predecessor (Node a NIL _) = error "Node doesn't contains predecessor"
predecessor (Node a left _) = maximumBST left

successor :: (Ord a) => BinaryTree a -> a
successor NIL = error "Node doesn't contains sucessor"
successor (Node a _ NIL) = error "Node doesn't contains sucessor"
successor (Node a _ right) = minimumBST right

removeBST :: (Ord a) => BinaryTree a -> a -> BinaryTree a
removeBST NIL a = NIL
removeBST (Node t left right) a 
    | a > t = Node t left (removeBST right a)
    | a < t = Node t (removeBST left a) right
    | otherwise = removeNode (Node t left right)

removeNode :: Ord a => BinaryTree a -> BinaryTree a
removeNode NIL = error "Node doesn't exist"
removeNode (Node t NIL NIL) = NIL
removeNode (Node t NIL right) = right
removeNode (Node t left NIL) = left
removeNode (Node t left right) = 
    Node (minimumBST right) left (removeBST right (minimumBST right))

order :: (Ord a) => BinaryTree a -> [a]
order NIL = []
order (Node a left right) = order left ++ [a] ++ order right

preOrder NIL = []
preOrder (Node t left right) = [t] ++ preOrder left ++ preOrder right

postOrder NIL = []
postOrder (Node a left right) = postOrder left ++ postOrder right ++ [a]