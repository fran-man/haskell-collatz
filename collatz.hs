import Data.Maybe
import GHC.Base (neChar)
threeNplusOne :: Int -> Int
threeNplusOne n = 3*n + 1

divideByTwo :: Int -> Maybe Int
divideByTwo n
    | even n    = Just (n `div` 2)
    | otherwise = Nothing

collatzNext :: Int -> Maybe Int
collatzNext n
    | even n = divideByTwo n
    | otherwise = Just (threeNplusOne n)

collatzSeq :: Int -> [Int] -> Maybe [Int]
collatzSeq n current
    | n == 2 = sequence (map Just current ++ [Just 2, collatzNext n])
    | otherwise = do
            next <- (collatzNext n)
            collatzSeq next (current ++ [n])