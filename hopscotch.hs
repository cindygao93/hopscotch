--A collection of functions that operates on haskell lists

module Assignment2 where

import Prelude


--hopscotch function that takes a list and returns the path that has the greatest sum. 
--There will be a list of paths generated due to te path jumping one or two values within the list
hopscotch :: [Int] -> ([Int], Int)
hopscotch alist = bestPath (paths alist)

--takes a list as a parameter and returns a list of the different pathsways of the list
paths :: [Int] -> [[Int]]
paths [] = [[]]
paths [a] = [[a]]
paths [a,b] = [[a]]
paths (a:b:c:as) = [a:y| y<-(paths (c:as))] ++ [a:y|y<-(paths (as))]

--takes a list of pathways and determines the best path by summing the value of 
--the elements in each list and returning the one with the greatest sum
--return value is a tuple consisting of the pathway and it's sum
bestPath :: [[Int]] ->([Int], Int)
bestPath [[]] = ([], 0)
bestPath [x] = (x, sum x)
bestPath (x:y:xs) = bestPath ((maxList x y) : xs)

--helper function for bestPaths that compares two lists, computes the sum of the elements in each list
--and returns the one that has the greater sum. If their sums are equal, returns the first inputted list
maxList :: [Int] -> [Int] -> [Int]
maxList first second
    | sum first >= sum second = first
    | sum first < sum second = second

