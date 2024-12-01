module Main where
import Data.Functor
import Data.List

readInt :: String -> Int
readInt = read

readFromInput :: IO ([Int], [Int])
readFromInput = do 
    f <- readFile "input.txt" <&> transpose . map words . lines
    return (map readInt (head f), map readInt (f !! 1))


red :: [Int] -> [(Int, Int)] -> Int
red [] _ = 0
red _ [] = 0
red (x:xs) (a@(y, n):ys)| x == y   = x*n + red xs ys 
                      | x < y    = red xs (a:ys)
                      |otherwise = red (x:xs) ys 
-- red (x:xs) (x:ys) = 

q1 :: IO ()
q1 = do
    (a, b) <- readFromInput
    print $ sum $ zipWith (\x y -> abs (x - y)) (sort a) (sort b)

removeDubs :: Ord a => [a] -> [a]
removeDubs = map head . group . sort

findGroups :: Ord a => [a] -> [(a, Int)]
findGroups = map (\b -> (head b,  length b)) . group . sort

q2 :: IO ()
q2 = do
    (a, b) <- readFromInput
    print $ red (removeDubs a) (findGroups b)


main :: IO ()
main = q2
