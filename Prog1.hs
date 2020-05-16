{- ##########################
   Mark Richmond
   Homework 1
   ########################## -}

module Prog1 where

--done
isSingleDigit :: Integer -> Bool
isSingleDigit n
  | n < 10 && n > (-10) = True
  | otherwise = False

--done
dividesEvenly :: Integer -> Integer -> Bool
dividesEvenly a b
  | (a `mod` b) == 0 = True
  | otherwise = False

--done
middle :: Integer -> Integer -> Integer -> Integer
middle a b c
  | (a >= b && a <= c) || (a <= b && a >= c) = a
  | (b >= a && b <= c) || (b <= a && b >= c)  = b
  | otherwise =  c

--done
nand :: Bool -> Bool -> Bool
nand b1 b2 = if (b1 == True && b2 == True) then False else True

--done
triangleArea :: Integer -> Integer -> Float
triangleArea b h = (1/2) * (fromIntegral b) * (fromIntegral h)

--done
floorDecimal :: Float -> Float
floorDecimal n = fromIntegral (floor n)

--done
isNotALetter :: Char -> Bool
isNotALetter c
  | c < 'A' = True
  | c > 'Z' && c < 'a' = True
  | c > 'z' = True
  | otherwise = False

--done
letterGrade :: Integer -> String
letterGrade g
  | g >= 93 = "A"
  | g >= 90  = "A-"
  | g >= 87  = "B+"
  | g >= 83  = "B"
  | g >= 80  = "B-"
  | g >= 77  = "C+"
  | g >= 73  = "C"
  | g >= 70  = "C-"
  | g >= 67  = "D+"
  | g >= 63  = "D"
  | g >= 60  = "D-"
  | otherwise = "F"

--done
averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c = (fromIntegral (a+b+c) / 3)

--done
howManyBelowAverage :: Integer -> Integer -> Integer -> Integer
howManyBelowAverage a b c
  | (a < (ceiling (averageThree a b c))) && (b < (ceiling (averageThree a b c))) = 2
  | (a < (ceiling (averageThree a b c))) && (c < (ceiling (averageThree a b c))) = 2
  | (b < (ceiling (averageThree a b c))) && (c < (ceiling (averageThree a b c))) = 2
  | a < (ceiling (averageThree a b c)) = 1
  | b < (ceiling (averageThree a b c)) = 1
  | c < (ceiling (averageThree a b c)) = 1
  | otherwise = 0

