{- ###########################################
   Mark Richmond
   Homework 4
   #########################################-}

module Prog4 where

--done
morerecent :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
morerecent (m1, d1, y1) (m2, d2, y2)
  | y1 > y2 = (m1, d1, y1)
  | y1 == y2 && m1 > m2 || y1 == y2 && m1 == m2 && d1 > d2  = (m1, d1, y1)
  | otherwise = (m2, d2, y2)

--helper
getMonth :: (Int, Int, Int) -> Int
getMonth (m, _, _) = m

--done
numInMonth :: [(Int, Int, Int)] -> Int -> Int
numInMonth [] _ = 0
numInMonth list month
  | getMonth (head list) == month = 1 + numInMonth (tail list) month
  | otherwise = numInMonth (tail list) month

--done
datesInMonth :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
datesInMonth [] _ = []
datesInMonth list month
  | getMonth (head list) == month = (head list):(datesInMonth (tail list) month)
  | otherwise = datesInMonth (tail list) month

--done
month2Str :: (Int, Int, Int) -> String
month2Str (m, _, _) =
  case m of
  1 -> "January"
  2 -> "February"
  3 -> "March"
  4 -> "April"
  5 -> "May"
  6 -> "June"
  7 -> "July"
  8 -> "August"
  9 -> "September"
  10-> "October"
  11-> "November"
  12-> "December"

--done
date2Str :: (Int, Int, Int) -> String
date2Str (m, d, y) = month2Str (m, d, y) ++ " " ++ (show d) ++ ", " ++ (show y)

--done
monthLookup :: Int -> Int
monthLookup day
  | 1 <= day && day <= 31 = 1
  | 31 < day && day <= 59 = 2
  | 59 < day && day <= 90 = 3
  | 90 < day && day <= 120 = 4
  | 120 < day && day <= 151 = 5
  | 151 < day && day <= 181 = 6
  | 181 < day && day <= 212 = 7
  | 212 < day && day <= 243 = 8
  | 243 < day && day <= 273 = 9
  | 273 < day && day <= 304 = 10
  | 304 < day && day <= 334 = 11
  | 334 < day && day <= 365 = 12

--done
monthRange :: Int -> Int -> [Int]
monthRange st end = [(monthLookup st)..(monthLookup end)]

--done
validDate :: (Int, Int, Int) -> Bool
validDate (m, d, y)
  | d < 1 = False
  | m == 1 && d <= 31 = True
  | m == 2 && d <= 28 = True
  | m == 3 && d <= 31 = True
  | m == 4 && d <= 30 = True
  | m == 5 && d <= 31 = True
  | m == 6 && d <= 30 = True
  | m == 7 && d <= 31 = True
  | m == 8 && d <= 31 = True
  | m == 9 && d <= 30 = True
  | m == 10 && d <= 31 = True
  | m == 11 && d <= 30 = True
  | m == 12 && d <= 31 = True
  | otherwise = False

--done
validLeapDate :: (Int, Int, Int) -> Bool
validLeapDate (m, d, y)
  | m /= 2 = False
  | d /= 29 = False
  | y `mod` 100 == 0 = False
  | y `mod` 400 == 0 || y `mod` 4 == 0 = True

--done
season :: (Int, Int, Int) -> String
season (m, d, y)
  | m == 3 && d >= 20 = "Spring"
  | m == 4 || m == 5 = "Spring"
  | m == 6 && d <= 20 = "Spring"
  | m == 6 && d > 20 = "Summer"
  | m == 7 || m == 8 = "Summer"
  | m == 9 && d <= 22 = "Summer"
  | m == 9 && d > 22 = "Fall"
  | m == 10 || m == 11 = "Fall"
  | m == 12 && d <= 21 = "Fall"
  | otherwise = "Winter"
