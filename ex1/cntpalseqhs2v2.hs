import Control.Monad.ST
import Data.Array.ST
import Data.Array
import GHC.Int

stringToArray :: String -> Array Int Char
stringToArray s = listArray (0, length s - 1) s

-- tls = tail . reverse . tails
-- buildPair :: String -> Int -> ST s Integer
buildPair str x = do
    arr <- newArray ((0,0),(x-1,x-1)) 0 :: ST s (STArray s (Int,Int) Integer)
    diagon arr (x-1)  -- init diagonal with 1's
    paranormal_populate (stringToArray str) arr (x-1) (x-2,x-1)
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
        writeArray arr (i,j) ((a + b + if ((str ! i) == (str ! j))
                then 1
                else -c) )
    | j == x = do
        a <- readArray arr (i+1,j)
        b <- readArray arr (i,j-1)
        c <- readArray arr (i+1,j-1)
        writeArray arr (i,j) ((a + b + if ((str ! i) == (str ! j))
                then 1
                else -c) )
        paranormal_populate str arr x (i-1,i)
    | otherwise = do
        a <- readArray arr (i+1,j)
        b <- readArray arr (i,j-1)
        c <- readArray arr (i+1,j-1)
        writeArray arr (i,j) ((a + b + if ((str ! i) == (str ! j))
                then 1
                else -c) )
        paranormal_populate str arr x (i,j+1)


solve :: [String] -> Integer
solve (x:str:[]) = (runST $ buildPair str ((read x)::Int)) `rem` 20130401

main :: IO ()
main = interact $ (\x->x++"\n") . show . solve . words


-- Sources:
-- http://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array-ST.html
-- https://wiki.haskell.org/Arrays
