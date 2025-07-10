-- Author: Jehadur Rahman
-- Organization: CyArm
-- GitHub: https://github.com/JehadurRE
-- Website: jehadurre.me
-- Date: 2025-07-10 09:43 (Thu)

-- by Kirill Elagin

fibonacci_last_digit :: Int -> Int
fibonacci_last_digit = helper (0, 1)
  where
    helper (a, _) 0 = a `mod` 10
    helper (a, b) n = helper (b `mod` 10, (a + b) `mod` 10) (n - 1)

main :: IO ()
main = do
  [n] <- fmap words getLine
  print $ fibonacci_last_digit (read n)
