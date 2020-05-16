{-#######################################
Mark Richmond
Homework 3
#######################################-}

module Prog3 where
import Data.Char

--done
productLastPart :: Int -> [Int] -> Int
productLastPart n list = product (snd (splitAt (n-1) list))

--done
init' :: [Int] -> [Int]
init' xs = [x | x <- xs, x /= last xs]

--done
init'' :: [Int] -> [Int]
init'' (x:xs) 
  | length xs == 0 = [] --base case
  | otherwise = (x:init'' xs)

--done
elemAt :: Int -> [Int] -> Int
elemAt n (x:xs)
  | n == x = 1
  | n /= x = 1 + (elemAt n xs)

--done
numTimes :: Int -> [Int] -> Int
numTimes n (x:xs)
  | xs == [] && n /= x = 0 --base case, last /= n
  | xs == [] && n == x = 1 --base case, last == n
  | n == x = 1 + (numTimes n xs)
  | n /= x = numTimes n xs

--done
lowerFirstLetter :: String -> String
lowerFirstLetter str = (toLower (head str):tail str)

--done
nestedParens :: String -> Bool
nestedParens str
  | str == "" = True --base case
  | even (length str) && head str == '(' && last str == ')' = nestedParens (init (tail str))
  | otherwise = False

--done
triads :: Int -> [(Int, Int, Int)]
triads n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x <= n && y <= n && z <= n, (x*x) + (y*y) == (z*z)]

--helper iSort
middle :: (Float, Int, String) -> Int
middle (_, x, _) = x

--helper iSort
ins :: (Float, Int, String) -> [(Float, Int, String)] -> [(Float, Int, String)]
ins elem [] = [elem] --base case
ins elem list
  | (middle elem) < (middle (head list)) = elem:list
  | otherwise = (head list):(ins elem (tail list))

--done
iSort' :: [(Float, Int, String)] -> [(Float, Int, String)]
iSort' [] = []
iSort' list = ins (head list) (iSort' (tail list))

--done
merge :: [Int] -> [Int] -> [Int]
merge (x:xs) (y:ys)
  | x > y && xs == [] = x:(y:ys) --base case, run out of xs
  | y >= x && ys ==[] = y:(x:xs) --base case, run out of ys
  | x > y = x:(merge xs (y:ys))
  | y >= x = y:(merge (x:xs) ys)
