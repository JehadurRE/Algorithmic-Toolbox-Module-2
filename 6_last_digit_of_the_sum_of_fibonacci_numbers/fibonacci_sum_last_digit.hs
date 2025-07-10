-- Author: Jehadur Rahman
-- Organization: CyArm
-- GitHub: https://github.com/JehadurRE
-- Website: jehadurre.me
-- Date: 2025-07-10 09:43 (Thu)

-- by Kirill Elagin

fibonacci_sum_naive :: Integer -> Int
fibonacci_sum_naive = helper (0, 1, 0)
  where
    helper (_, _, s) 0 = s `mod` 10
    helper (a, b, s) i = helper (b, a + b, s + b) (i - 1)

fibonacci_sum_fast :: Integer -> Int
fibonacci_sum_fast n = helper (0, 1, 1) (n' - 1)
  where
    n' = n `mod` 60  -- Pisano period for mod 10
    helper (_, _, s) 0 = s `mod` 10
    helper (a, b, s) i = helper (b `mod` 10, (a + b) `mod` 10, (s + (b `mod` 10)) `mod` 10) (i - 1)

main :: IO ()
main = do
  [n] <- fmap words getLine
  print $ fibonacci_sum_fast (read n)
