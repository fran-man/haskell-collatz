import Data.Maybe
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

collatzSeq :: Maybe Int -> [Maybe Int] -> [Maybe Int]
collatzSeq n current
    | n == Just 2 = current ++ [Just 2, n >>= collatzNext]
    | otherwise = collatzSeq (n >>= collatzNext) (current ++ [n])