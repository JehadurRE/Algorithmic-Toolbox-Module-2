-- Author: Jehadur Rahman
-- Organization: CyArm
-- GitHub: https://github.com/JehadurRE
-- Website: jehadurre.me
-- Date: 2025-07-10 09:43 (Thu)

-- by Kirill Elagin

fibonacci_partial_sum_naive :: Integer -> Integer -> Int
fibonacci_partial_sum_naive from to = let (a', b', _) = helper (0, 1, 0) from
                                          (_, _, s) = helper (a', b', a') (to - from)
                                      in s
  where
    helper (a, b, s) 0 = (a, b, s `mod` 10)
    helper (a, b, s) i = helper (b, a + b, s + b) (i - 1)

fibonacci_partial_sum_fast :: Integer -> Integer -> Int
fibonacci_partial_sum_fast from to = helper (0, 1, 0) 0
  where
    from' = from `mod` 60
    to' = if (to `mod` 60) < from' then (to `mod` 60) + 60 else to `mod` 60
    helper (current, next, sum) i
      | i > to' = sum `mod` 10
      | i >= from' = helper (next, (current + next) `mod` 10, (sum + current) `mod` 10) (i + 1)
      | otherwise = helper (next, (current + next) `mod` 10, sum) (i + 1)

main :: IO ()
main = do
  [from, to] <- fmap words getLine
  print $ fibonacci_partial_sum_fast (read from) (read to)
