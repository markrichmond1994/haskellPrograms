{- #################################
   Mark Richmond
   Homework 7.
   ###############################-}

module Prog7 where

--done
unique :: Eq a => [a] -> [a]
unique (x:xs)
  | xs == [] = x:xs
  | any(x==) xs = unique xs
  | otherwise = x:(unique xs)

data Expr1 = Val1 Int
           | Add1 Expr1 Expr1
           | Sub1 Expr1 Expr1

--done
value1 :: Expr1 -> Int
value1 (Val1 n) = n
value1 (Add1 e1 e2) = (value1 e1) + (value1 e2)
value1 (Sub1 e1 e2) = (value1 e1) - (value1 e2)

data Expr2 = Val2 Int
           | Mul2 Expr2 Expr2
           | Div2 Expr2 Expr2
           | Add2 Expr2 Expr2
           | Sub2 Expr2 Expr2

--helper
helper :: Maybe Int -> Int
helper (Just n) = n
helper Nothing = 0
--done
value2 :: Expr2 -> Maybe Int
value2 (Val2 n) = Just n
value2 (Mul2 e1 e2) = Just (helper (value2 e1) * helper (value2 e2))
value2 (Div2 e1 e2)
  | helper (value2 e2) == 0 = Nothing
  | otherwise = Just ((helper (value2 e1)) `div` (helper (value2 e2)))
value2 (Add2 e1 e2) = Just (helper (value2 e1) + helper (value2 e2))
value2 (Sub2 e1 e2) = Just (helper (value2 e1) - helper (value2 e2))

--helper
isAVowel :: Char -> Bool
isAVowel c
  | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = True
  | otherwise = False
--fix :(
piglatinize :: String -> String
piglatinize (c:cs)
  | isAVowel c = (c:cs) ++ "ay"
  | otherwise = piglatinize (cs ++ [c])

data Tree a = Leaf a
            | Node (Tree a)(Tree a)

--helper
leaves :: Tree a -> Int
leaves (Leaf n) = 1
leaves (Node l r) = leaves l + leaves r
--done
balanced :: Tree a -> Bool
balanced (Leaf n) = True
balanced (Node l r)
  | leaves l - leaves r == 1 = True
  | leaves l - leaves r == (-1) = True
  | leaves l - leaves r == 0 = True
  | otherwise = False

data Expr3 = Val3 Int
           | Mul3 Expr3 Expr3
           | Div3 Expr3 Expr3
           | Add3 Expr3 Expr3
           | Sub3 Expr3 Expr3
           | If BExpr3 Expr3 Expr3

data BExpr3 = BoolLit Bool
            | Or BExpr3 BExpr3
            | EqualTo Expr3 Expr3
            | LessThan Expr3 Expr3

--done
bEval :: BExpr3 -> Bool
bEval (BoolLit True) = True
bEval (BoolLit False) = False
bEval (Or b1 b2)
  | bEval b1 = True
  | bEval b2 = True
  | otherwise = False
bEval (EqualTo b1 b2)
  | helper (value3 b1) == helper(value3 b2) = True
  | otherwise = False
bEval (LessThan b1 b2)
  | helper (value3 b1) < helper (value3 b2) = True
  | otherwise = False

--done
value3 :: Expr3 -> Maybe Int
value3 (Val3 n) = Just n
value3 (Mul3 e1 e2) = Just (helper (value3 e1) * helper (value3 e2))
value3 (Div3 e1 e2)
  | helper (value3 e2) == 0 = Nothing
  | otherwise = Just (helper (value3 e1) `div` helper (value3 e2))
value3 (Add3 e1 e2) = Just (helper (value3 e1) + helper (value3 e2))
value3 (Sub3 e1 e2) = Just (helper (value3 e1) - helper (value3 e2))
value3 (If bExpr e1 e2)
  | bEval bExpr = Just (helper (value3 e1))
  | otherwise = Just (helper (value3 e2))
