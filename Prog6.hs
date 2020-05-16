{- #########################
Mark Richmond
Homework 6.
###########################-}

module Prog6 where

data Tree1 = Leaf1 Int
           | Node1 Int Tree1 Tree1

--done
preorder :: Tree1 -> [Int]
preorder (Leaf1 leaf) = [leaf]
preorder (Node1 v l r) = [v] ++ preorder l ++ preorder r

--done
postorder :: Tree1 -> [Int]
postorder (Leaf1 leaf) = [leaf]
postorder (Node1 v l r) = postorder l ++ postorder r ++ [v]

--done
sumPositives :: Tree1 -> Int
sumPositives tree = sum[t | t <- (preorder tree), t >= 0]

--helper for countLeaves
justLeaves :: Tree1 -> [Int]
justLeaves (Leaf1 leaf) = [leaf]
justLeaves (Node1 v l r) = justLeaves l ++ justLeaves r
--done
countLeaves :: Tree1 -> Int
countLeaves tree = length(justLeaves tree)

--helper depth, check depth of left child
justL :: Tree1 -> Int
justL (Leaf1 leaf) = 1
justL (Node1 v l r) = 1 + justL l
--helper depth, check depth of right child
justR :: Tree1 -> Int
justR (Leaf1 leaf) = 1
justR (Node1 v l r) = 1 + justR r
--done
depth :: Tree1 -> Int
depth tree
  | length(preorder tree) == 1 = 0 --just root? depth = 0
  | (justL tree) > (justR tree) = (justL tree) - 1 --check which child is longer
  | (justR tree) >= (justL tree) = (justR tree) - 1

data Tree2 a = Leaf2 a
             | Node2 [Tree2 a]

--done
occurs :: Eq a => a -> Tree2 a -> Bool
occurs n (Leaf2 leaf) = n == leaf
occurs n (Node2 x) = any(occurs n) x

--needs work
countInteriorNodes :: Tree2 a -> Int
countInteriorNodes (Leaf2 leaf) = 0
countInteriorNodes (Node2 tree) = 2

--done
sumTree :: Tree2 Int -> Int
sumTree (Leaf2 leaf) = leaf
sumTree (Node2 x) = sum (map sumTree x)

--done
pre2 :: Tree2 a -> [a]
pre2 (Leaf2 leaf) = [leaf]
pre2 (Node2 tree) = concatMap pre2 tree

--done
depthK :: Int -> Tree2 a -> [a]
depthK 0 (Leaf2 leaf) = [leaf]
depthK n (Node2 trees) = concatMap (depthK (n-1)) trees
depthK _ _ = []
