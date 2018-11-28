-- Γλώσσες Προγραμματισμού 2 2018
-- Ορέστης Καπαρουνάκης 03114057
-- Άσκηση 2 - Τα δέντρα, πώς να τα διπλώνετε και να τα κλαδεύετε
module Pruning where
  
data Tree a = T a [Tree a]
    deriving (Show)

tree = T 1 [T 2 [],
            T 3 [ T 4 [],
                  T 5 []],
            T 6 [ T 7 [],
                  T 8 [],
                  T 9 []]]

tree2 = T 1 [T 2 [T 4 [], T 5 []], T 3 []]

-- 1.
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (T a b) = f a $ map (foldTree f) b

-- pre order
-- foldTree (\x y -> foldl (++) [x] y) tree

-- post order
-- foldTree (\x y -> foldr (++) [x] y) tree

-- 2.
sizeTree :: Num b => Tree a -> b
sizeTree = foldTree (\x y -> foldr (+) 1 y)

heightTree :: (Ord b, Num b) => Tree a -> b
heightTree = foldTree (\x y -> 1 + foldr max 0 y)

sumTree :: Num a => Tree a -> a
sumTree = foldTree (\x y -> sum (x:y))

maxTree :: Ord a => Tree a -> a
maxTree = foldTree (\x y -> maximum (x:y))

inTree :: Eq a => a -> Tree a -> Bool
inTree x = foldTree (\z y -> or ((x == z):y))

nodes :: Tree a -> [a]
nodes = foldTree (\x y -> foldl (++) [x] y)   -- foldl displays them in preorder

countTree :: (a -> Bool) -> Tree a -> Integer
countTree f = foldTree (\x y -> sum ((if f x then 1 else 0):y))

leaves :: Tree a -> [a]
leaves = foldTree (\x y -> if (null y) then [x] else concat y)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree (\x y -> T (f x) y)

-- 3.

trimTree :: Int -> Tree a -> Tree a
trimTree 1 (T a b) = T a []
trimTree n (T a b) = T a $ map (trimTree (n-1)) b

path :: [Int] -> Tree a -> a
path [] (T a b) = a
path (ph:pt) (T a b) = path (pt) (b !! ph)
