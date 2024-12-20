module Main where
import Data.Functor
import Debug.Trace


readInt :: String -> Int
readInt = read

readFromInput :: IO [[Int]]
readFromInput = readFile "input.txt" <&>  map (map readInt . words) . lines

data Comparison = Positive | Negative
    deriving (Show, Eq)

mcompare :: Int -> Int -> Maybe Comparison
mcompare x y | x > y && x - y <= 3 = Just Negative
            | x < y && y - x <= 3 = Just Positive
            | otherwise = Nothing

eval :: [[Int]] -> Int
eval = sum . map (fromEnum . (all =<< (==). head) . (zipWith mcompare <*> tail))

empty :: [a] -> Bool
empty [] = True
empty _ = False  

reduce:: [Int] -> [Int]
reduce [] = []
reduce [x] = [x] 
reduce [x,_] = [x]
reduce (a:b:c:xs) | a*b > 0    = a: reduce' (b:c:xs)
                  | c == 0 = [a, -a + 5]
                  | a * c > 0 || b == 0 = (a + b):c:xs
                  | b * c > 0  || a == 0 = b:c:xs
                  | otherwise = undefined

reduce' :: [Int] -> [Int]
reduce' [] = []
reduce' [x] = [x]
reduce' [x,y] | x* y > 0 && abs y <= 3 = [x,y]
              | otherwise = [x]
reduce' (a:b:c:xs) | a*b == 0   = a:c:xs
                 | a*b > 0 && abs a <= 3  = a: reduce' (b:c:xs)
                 | a*b > 0 = b:c:xs
                 | otherwise  = a:(c + b): xs

cmp:: Int -> Maybe Comparison
cmp x | 1 <= x && x <= 3 = Just Positive
      | 1 <= -x && -x <= 3 = Just Negative
      | otherwise = Nothing

eval' :: [[Int]] -> Int
eval' = sum . map (fromEnum . (all =<< (==). head). map cmp. reduce . (zipWith (-) <*> tail))
-- eval' :: [[Int]] -> [[Int]]
-- eval' = take 10 . filter (not .(all =<< (==). head). map cmp. reduce . (zipWith (-) <*> tail))
-- --

q1 :: IO ()
q1 = do
    input <- readFromInput
    print $ eval input
 
q2 :: IO ()
q2 = do
    input <- readFromInput
    print $ eval' input


main :: IO ()
main = q2

