{- #######################
   Mark Richmond
   Homework 2
   ####################### -}

module Prog2 where

--done
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z
  | x /= y && y /= z = True
  | otherwise = False

--done
sum' :: Integer -> Integer
sum' n
  | n == 1 = 1
  | n > 1 = sum' (n-1) + n
  | otherwise = error "Invalid input"

--done
abssum :: Integer -> Integer -> Integer
abssum m n
  | m == n = abs m
  | n > m = abs n + abssum m (n-1)
  | otherwise = error "ERROR"

--done
integerSqrt :: Integer -> Integer
integerSqrt n = floor (sqrt (fromIntegral n))

--done
exponent' :: Integer -> Integer -> Integer
exponent' n m
  | m == 0 = 1
  | m > 0 = (exponent' n (m-1)) * n

--largeSmall helper max
threeMax :: Integer -> Integer -> Integer -> Integer
threeMax x y z = (max (max x y) z)

--largeSmall helper min
threeMin :: Integer -> Integer -> Integer -> Integer
threeMin x y z = (min (min x y) z)

--done
largeSmall :: (Integer, Integer, Integer) -> (Integer, Integer)
largeSmall (x, y, z) = (threeMax x y z, threeMin x y z)

--done
swap :: (Char, Char, Char, Char) -> (Char, Char, Char, Char)
swap (a, b, c, d) = (a, c, b, d)

--helper function for negateOdds
helperNegOdds :: Integer -> Integer
helperNegOdds n
  | odd n = (-n)
  | even n = n

--done
negateOdds :: [Integer] -> [Integer]
negateOdds n = [helperNegOdds x | x <- n]

--done
matches :: Integer -> [Integer] -> [Integer]
matches n list = [x | x <- list, x == n]

--done
element :: Integer -> [Integer] -> Bool
element n list
  | matches n list == [] = False
  | otherwise = True
