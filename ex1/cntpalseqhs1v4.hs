-- A[i,j] = A[i-1,j] + A[i,j-1] - A[i-1,j-1] +
-- (char(i) == char(j) ? A[i-1,j-1] + 1 : 0)

-- g a v t g v a    STR1
--   g a v t g v a  STR2
-- a v g t v a g
--       a v g t v a g

epic_palins str1 str2 pprev [a] = a
epic_palins str1 str2 pprev prev = epic_palins (tail str1) str2 (tail prev) next
    where
        next = help_epic str1 str2 pprev prev []

-- Actually, performance improved without zip
-- pprev and prev (we can't do without those)
-- IS tail recursive. BUT, has to REVERSE output, meaning totally O(n^2)
-- Should only work with utmost 4n elements
-- This (with reverse) actually performs better than not having Tail Recursion
help_epic [] _ _ _ next = reverse next
help_epic (a:sa) (b:sb) (hpp:rpp) (fp:sp:rest) now = help_epic sa sb rpp (sp:rest) next
  where
    next = seq front (front:now)
    front = (fp+sp+r) `mod` 20130401
    r = if a==b
      then 1
      else (-hpp)

solve :: [String] -> Int
solve (x:str:[]) = epic_palins (tail str) str (replicate (xr+1) 0) (replicate xr 1)
    where
      xr = read x

nl :: [Char] -> [Char]
nl x = x ++ "\n"

main :: IO ()
main = interact $  nl . show . solve . words

-- Sources:
-- https://www.reddit.com/r/haskell/comments/8hapb2/dynamic_programming_in_haskell_is_just_recursion/
-- http://travis.athougies.net/posts/2018-05-05-dynamic-programming-is-recursion.html
-- Memoization with Recursion https://wiki.haskell.org/Memoization
