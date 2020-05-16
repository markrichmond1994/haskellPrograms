{- ####################################################
   Mark Richmond
   Homework 5
   ##################################################-}

module Prog5 where

--done
reverse' :: [a] -> [a]
reverse' list = case list of
  [] -> []
  list -> (last list):reverse' (init list)

--done
isPalindrome :: String -> Bool
isPalindrome str = case (str == (reverse' str)) of
  True -> True
  False -> False

--done
safeFindAfter :: String -> [String] -> Maybe [String]
safeFindAfter _ [] = Nothing
safeFindAfter str (x:xs)
  | str == x = Just xs
  | otherwise = (safeFindAfter str xs)

data Set = Set [Char]
         | EmptySet
     deriving Show

--done
member :: Char -> Set -> Bool
member ch EmptySet = False
member ch (Set (c:cs))
  | ch /= c && cs == [] = False
  | ch == c = True
  | ch /= c = member ch (Set cs)

--done
size :: Set -> Int
size EmptySet = 0
size (Set []) = 0
size (Set (c:cs)) = 1 + size (Set cs)

--helper add
merge :: Set -> Set -> Set
merge (Set (x:xs)) (Set (y:ys)) = (Set (x:y:ys))

--done
add :: Char -> Set -> Set
add c EmptySet = (Set [c])
add c (Set cs)
  | member c (Set cs) = (Set cs)
  | otherwise = merge (Set [c]) (Set cs)

--done
equal :: Set -> Set -> Bool
equal (Set []) (Set []) = True
equal (Set (x:xs)) (Set (y:ys))
  | x == y = equal (Set xs) (Set ys)
  | otherwise = False
equal _ _ = False

--done
saferemove :: Char -> Set -> Maybe Set
saferemove c (Set []) = Nothing
saferemove c (Set list)
  | member c (Set list) == False = Nothing
saferemove c (Set xs) = Just (Set([x | x <- xs, x /= c]))

--done but might have duplicates
union :: Set -> Set -> Set
union (Set []) (Set []) = EmptySet
union (Set (x:xs)) (Set []) = (Set (x:xs))
union (Set []) (Set (y:ys)) = (Set (y:ys))
union (Set (x:xs)) (Set (y:ys)) = (Set ((x:xs) ++ (y:ys)))

--done
intersection :: Set -> Set -> Set
intersection (Set []) (Set []) = EmptySet
intersection (Set (x:xs)) (Set []) = EmptySet
intersection (Set []) (Set (y:ys)) = EmptySet
intersection (Set xs) (Set ys) = (Set[z | z <- xs, member z (Set ys)])
