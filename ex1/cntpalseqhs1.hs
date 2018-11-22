import Data.List
-- A[i,j] = A[i-1,j] + A[i,j-1] - A[i-1,j-1] +
-- (char(i) == char(j) ? A[i-1,j-1] + 1 : 0)

palins :: [Char] -> Int
palins []  = 0  -- Case of no letters (1 empty but it's added at top level)
palins [a] = 1  -- Case of one letter.
-- palins "ab" = 4
palins str = (normal_palins + rest)
    where
        normal_palins =  aleft + aright - adiag
        rest = if head str == last str
            then 1 + adiag
            else 0
        adiag = (palins $ init $ tail str)
        aleft = (palins $ tail str)
        aright = (palins $ init str)


-- start with ns = [1], ps = []


            -- string -> reversed string -> prevrow -> currow -> first char ->
-- advanced_palins :: [Char] -> [Char] -> [Int] -> [Int] -> Char -> Int

-- Args
-- The first char each time. This we get by the head of tails of string.
-- The previous line.

-- tls has all the tails of the string

tls = tail . reverse . tails
adv_pals :: [[Char]] -> [Int] -> Int
adv_pals [] lst = last lst
adv_pals tlsdstrings@(newstring:later) prevline = adv_pals later newline
      where
        newline = calcline (head newstring) newstring prevline

-- calcline: Takes as arguments; a letter, a string, an int list and returns an
-- int list base on this formula
-- out[i] = in[i] + out[i-1] + if (char==str[i]) then 1 else -in[i-1]
calcline :: Char -> [Char] -> [Int] -> [Int]
calcline c str prv = calchelp c 0 0 str prv
    where
      calchelp :: Char -> Int -> Int -> [Char] -> [Int] -> [Int]
      calchelp _ mem inprev ""  [] = []
      calchelp c mem inprev str prv@(h:r) = x:(calchelp c x h (tail str) r)
        where
          x = h + mem + if c == head str
            then 1
            else (-inprev)

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
