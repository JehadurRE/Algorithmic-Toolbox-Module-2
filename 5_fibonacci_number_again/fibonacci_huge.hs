-- Author: Jehadur Rahman
-- Organization: CyArm
-- GitHub: https://github.com/JehadurRE
-- Website: jehadurre.me
-- Date: 2025-07-10 09:43 (Thu)

-- by Kirill Elagin

get_fibonacci_huge_naive :: Integer -> Int -> Int
get_fibonacci_huge_naive n m = helper (0, 1) n
  where
    helper (a, _) 0 = a `mod` m
    helper (a, b) i = helper (b, a + b) (i - 1)

findPisanoPeriod :: Int -> Integer
findPisanoPeriod m = helper 0 1 0
  where
    helper prev current i
      | prev == 0 && current == 1 && i > 0 = i
      | i >= fromIntegral (m * m) = 0
      | otherwise = helper current ((prev + current) `mod` fromIntegral m) (i + 1)

get_fibonacci_huge_fast :: Integer -> Int -> Int
get_fibonacci_huge_fast n m = helper (0, 1) (n `mod` p)
  where
    p = findPisanoPeriod m
    helper (a, _) 0 = a `mod` m
    helper (a, b) i = helper (b `mod` fromIntegral m, (a + b) `mod` fromIntegral m) (i - 1)

main :: IO ()
main = do
  [n, m] <- fmap words getLine
  print $ get_fibonacci_huge_fast (read n) (read m)
