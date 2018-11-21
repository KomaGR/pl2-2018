-- A[i,j] = A[i-1,j] + A[i,j-1] - A[i-1,j-1] +
-- (char(i) == char(j) ? A[i-1,j-1] + 1 : 0)

palins :: [Char] -> Int
palins []  = 0  -- Case of no letters (1 empty but it's added at top level)
palins [a] = 1  -- Case of one letter.
palins "ab" = 4
palins str = (normal_palins + rest)
      where
        normal_palins =  aleft + aright - adiag
        rest = if head str == last str
            then 1 + adiag
            else 0
        adiag = (palins $ init $ tail str)
        aleft = (palins $ tail str)
        aright = (palins $ init str)


-- -- Memoization with Recursion https://wiki.haskell.org/Memoization
-- slow_fib :: Int -> Integer
-- slow_fib 0 = 0
-- slow_fib 1 = 1
-- slow_fib n = slow_fib (n-2) + slow_fib (n-1)
-- -- The memoized version is much faster. Try memoized_fib 10000.
--
-- memoized_fib :: Int -> Integer
-- memoized_fib = (map fib [0 ..] !!)
--    where fib 0 = 0
--          fib 1 = 1
--          fib n = memoized_fib (n-2) + memoized_fib (n-1)
