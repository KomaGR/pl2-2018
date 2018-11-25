-- A[i,j] = A[i-1,j] + A[i,j-1] - A[i-1,j-1] +
-- (char(i) == char(j) ? A[i-1,j-1] + 1 : 0)

-- Recursive solution with dictionary memoization.

-- g a v t g v a    STR1
--   g a v t g v a  STR2
-- a v g t v a g
--       a v g t v a g

epic_palins str1 str2 pprev [a] = a
epic_palins str1 str2 pprev prev = epic_palins (tail str1) str2 (tail prev) next
    where
        next = help_epic (zip str1 str2) pprev prev

help_epic [] _ _ = []
help_epic ((a,b):sr) (hpp:rpp) p@(fp:sp:rest) =
  (fp+sp+r):(help_epic sr rpp (sp:rest))
  where
    r = if a==b
      then 1
      else (-hpp)

solve :: [String] -> Integer
solve (x:str:[]) = epic_palins (tail str) str (replicate (xr+1) 0) (replicate xr 1)
    where
      xr = read x

νέαγραμμή :: [Char] -> [Char]
νέαγραμμή x = x ++ "\n"

main :: IO ()
main = interact $  νέαγραμμή . show . (\x -> x `rem` 20130401) . solve . words

-- Sources:
-- https://www.reddit.com/r/haskell/comments/8hapb2/dynamic_programming_in_haskell_is_just_recursion/
-- http://travis.athougies.net/posts/2018-05-05-dynamic-programming-is-recursion.html
-- Memoization with Recursion https://wiki.haskell.org/Memoization
