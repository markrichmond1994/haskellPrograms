{-
   Prog6.hs
   Tom Barrett, April 2019
   Dr. Richard Burns
   CSC335: Programming Language Concepts and Paradigms
-}

module Prog6 where

data Tree1 = Leaf1 Int
            | Node1 Tree1 Int Tree1

t1 :: Tree1
t1 = Node1 (Node1 (Leaf1 2) 1 (Leaf1 3)) 4 (Node1 (Leaf1 5) 6 (Leaf1 7))

-- takes a tree argument and returns as a list an inorder traversal of the tree.
preorder :: Tree1 -> [Int]
preorder (Leaf1 x) = [x]
preorder (Node1 left n right) = n : preorder left ++ preorder right

-- takes a tree argument and returns as a list an inorder traversal of the tree.
postorder :: Tree1 -> [Int]
postorder (Leaf1 x) = [x]
postorder(Node1 left n right) = postorder left ++ postorder right ++ [n]

-- takes a tree argument and returns the sum of positive integers in the tree.
sumpositives :: Tree1 -> Int
sumpositives (Leaf1 x)
    | x > 0 = x
    | otherwise = 0
sumpositives (Node1 left n right)
    | n > 0 = n + (sumpositives left) + (sumpositives right)
    | otherwise = (sumpositives left) + (sumpositives right)

-- returns the number of interior nodes in the given tree.
countInteriorNodes :: Tree1 -> Int
countInteriorNodes (Leaf1 a) = 0
countInteriorNodes (Node1 left n right) = 1 + countInteriorNodes right + countInteriorNodes left

-- returns the depth of a tree. (A tree with only a root node is defined to have depth=1.)
depth :: Tree1 -> Int
depth (Leaf1 _) = 1
depth (Node1 left n right) = 1 + max (depth left) (depth right)

-- general tree, whose interior nodes can have an arbitrary number of children
data Tree2 a = Leaf2 a
             | Node2 [Tree2 a]

t2 :: Tree2 Int
t2 =  Node2[Leaf2 0, Node2 [], Node2 [Leaf2 2, Leaf2 3, Leaf2 4, Node2 [Leaf2 5, Leaf2 6]], Node2 [Node2 [Leaf2 5, Leaf2 6], Leaf2 2, Leaf2 3, Leaf2 4]]

--(>>=)  :: m a -> (  a -> m b) -> m b
-- converts Tree2 into a list
flatten :: Tree2 a -> [a]
flatten (Leaf2 a) = [a]
flatten (Node2 xs) = xs >>= flatten

-- returns whether a given argument is present in a given tree.
occurs :: Eq a => a -> Tree2 a -> Bool
occurs x (Leaf2 y) = x == y
occurs x (Node2 n) = any (occurs x) n

-- takes a tree argument and returns the number of leaves in the tree.
countLeaves :: Tree2 a -> Int
countLeaves (Leaf2 a) = 1
countLeaves (Node2 a) = sum (map countLeaves a)

-- takes a tree of integers and returns the sum of all integers in the tree.
sumTree :: Tree2 Int -> Int
sumTree (Leaf2 a) = a
sumTree (Node2 a) = sum (map sumTree a)

-- returns a postorder traversal of the nodes in the tree.
post2 :: Tree2 a -> [a]
post2 (Leaf2 a) = [a]
post2 (Node2 xs) = concatMap post2 xs

--returns all nodes that are at depth k in the tree. (A tree with only a root node is defined to have depth=1.)
-- The order that the nodes are returned does not matter
depthK :: Int -> Tree2 a -> [a]
depthK 1 (Leaf2 a) = [a]
depthK n (Node2 xs) = concatMap (depthK (n-1)) xs