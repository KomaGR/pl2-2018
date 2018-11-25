import Control.Monad.ST
import Data.Array.ST
import Data.Array


-- buildPair :: String -> Int -> ST s Int
buildPair str x = do
    arr <- newArray ((0,0),(x-1,x-1)) 0 :: ST s (STUArray s (Int,Int) Int)
    diagon arr (x-1)
    paranormal_populate str arr (x-1) (x-2,x-1)
    b <- readArray arr (0,x-1)
    return b

diagon arr x
    | x == 0 = do
        writeArray arr (0,0) 1
    | otherwise = do
        writeArray arr (x,x) 1
        diagon arr (x-1)

paranormal_populate str arr x (i,j)
    | j == x && i == 0 = do
        a <- readArray arr (i+1,j)
        b <- readArray arr (i,j-1)
        c <- readArray arr (i+1,j-1)
        writeArray arr (i,j) (a + b + if ((str !! i) == (str !! j))
                then 1
                else -c)
    | j == x = do
        a <- readArray arr (i+1,j)
        b <- readArray arr (i,j-1)
        c <- readArray arr (i+1,j-1)
        writeArray arr (i,j) (a + b + if ((str !! i) == (str !! j))
                then 1
                else -c)
        paranormal_populate str arr x (i-1,i)
    | otherwise = do
        a <- readArray arr (i+1,j)
        b <- readArray arr (i,j-1)
        c <- readArray arr (i+1,j-1)
        writeArray arr (i,j) (a + b + if ((str !! i) == (str !! j))
                then 1
                else -c)
        paranormal_populate str arr x (i,j+1)

-- paranormal_populate str arr x (i,j)
--   | j == x && i == 0 = do
--       a <- readArray arr (i+1,j)
--       b <- readArray arr (i,(j-1))
--       c <- if i == j
--           then 1
--           else -(readArray arr (i+1,j-1))
--       writeArray arr (i,j) (a+b+c)
--   | otherwise = do
--       a <- readArray arr (i+1,j)
--       b <- readArray arr (i,j-1)
--       c <- if i == j
--         then 1
--         else -(readArray arr (i+1,j-1))
--       writeArray arr (i,j) (a+b+c)
--       if j == x
--           then paranormal_populate str arr x (i-1,i)
--           else paranormal_populate str arr x (i,j+1)


solve :: (Int,String) -> Int
solve (x,str) = runST $ buildPair str x

-- main :: IO ()
-- main = interact $ show . solve .  words

-- A[i,j] = A[i-1,j] + A[i,j-1] - A[i-1,j-1] +
-- (char(i) == char(j) ? A[i-1,j-1] + 1 : 0)

-- Recursive solution (probably bad).

-- palins :: [Char] -> Int
-- palins []  = 0  -- Case of no letters (1 empty but it's added at top level)
-- palins [a] = 1  -- Case of one letter.
-- -- palins "ab" = 4
-- palins str = (normal_palins + rest)
--     where
--         normal_palins =  aleft + aright - adiag
--         rest = if head str == last str
--             then 1 + adiag
--             else 0
--         adiag = (palins $ init $ tail str)
--         aleft = (palins $ tail str)
--         aright = (palins $ init str)


-- Sources:
-- https://www.reddit.com/r/haskell/comments/8hapb2/dynamic_programming_in_haskell_is_just_recursion/
-- http://travis.athougies.net/posts/2018-05-05-dynamic-programming-is-recursion.html
-- Memoization with Recursion https://wiki.haskell.org/Memoization
