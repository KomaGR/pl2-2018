import Control.Monad.ST
import Data.Array.ST

buildPair = do
    arr <- newArray (1,10) 4 :: ST s (STArray s Int Int)
    a <- readArray arr 1
    writeArray arr 1 42
    b <- readArray arr 1
    c <- readArray arr 2
    return (a,b,c)


main :: IO ()
main = print $ runST $ buildPair

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
