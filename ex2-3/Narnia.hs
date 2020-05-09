-- Γλώσσες Προγραμματισμού 2 2018
-- Ορέστης Καπαρουνάκης 03114057
-- Άσκηση 3 - Το πουλί, το άπειρο δέντρο και οι γρήγορες δοκιμές
module Narnia where

import Pruning
import Test.QuickCheck
import Data.Ratio

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbitrarySizeTree


arbitrarySizeTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizeTree m = do
    a <- arbitrary
    n <- choose (0, m)
    as <- vectorOf n (arbitrarySizeTree (m `div` 4))
    return (T a as)

-- 2. Check properties

prop_HeightNaturalAndLessThanSize tree = (ht > 0) && (ht <= sizeTree tree)
                                            where
                                                ht = heightTree tree

prop_MaxTreeIsInTree tree = inTree (maxTree tree) tree

prop_EveryNodeIsInTree tree = and $ map (`inTree` tree) $ nodes tree

-- prop_CountIsNaturalAndLessThanSize :: (Tree a) -> (Blind (a -> Bool)) -> Bool
-- prop_CountIsNaturalAndLessThanSize tree (Blind f) = (ct > 0) && (ct <= sizeTree tree)
-- prop_CountIsNaturalAndLessThanSize :: (Tree Int) -> (Int -> Bool) -> Bool
prop_CountIsNaturalAndLessThanSize tree (Blind f) = (ct > 0) && (ct <= sizeTree tree)
                                                    where
                                                        ct = countTree f tree
-- Φεϊλάρει για a :: (), αλλά αυτό δεν θα έπρεπε να συμβαίνει αν f :: () -> Bool
-- e.g. try this in ghci:
-- iotree = generate arbitrary :: IO (Tree ())
-- tree <- iotree
-- countTree (\x -> True) tree
-- countTree (\x -> False) tree

prop_NodesIsSizeAndLeavesIsLess tree =
    ((length $ nodes tree) == st) &&
    (ll < st || (ll == 1 && st == 1))
        where
            ll = length $ leaves tree
            st = sizeTree tree

-- prop_MapRetainsSizeHeight :: Tree a1 -> (a1 -> Int) -> Bool
prop_MapRetainsSizeHeight tree (Blind f) =
    ((sizeTree tree == sizeTree nt) &&
    (heightTree tree == heightTree nt))
        where
            nt = mapTree f tree

prop_NInTreeImpliesNInMapTree tree (Blind f) n =
    inTree n tree == inTree (f n) (mapTree f tree)

prop_MapNodes tree (Blind f) =
    ((map f) . nodes)  tree == (nodes . (mapTree f)) tree

prop_MapLeaves tree (Blind f) =
    ((map f) . leaves)  tree == (leaves . (mapTree f)) tree


-- 3. Tree Bird - https://www.youtube.com/watch?v=D0W1v0kOELA

-- inverse :: Rational -> Rational
inverse x = (denominator x) % (numerator x)

bird :: Tree Rational
bird = T (1 % 1)
             [mapTree (\x -> (toRational . inverse) (x + 1)) bird,
              mapTree (\x -> toRational ((inverse x) + 1)) bird]

-- 4. Check the bird

prop_NLongPath boolpath =
        let intpath = map (\x -> if x then 1 else 0) boolpath
            n = length intpath
        in path intpath bird == path intpath (trimTree (n+1) bird) -- n+1 because n is #edges, not #nodes


prop_ZigZag (NonNegative n) = 
        let boolpath = [odd x | x <- [1..(n)]]
            zigzag = map (\x -> if x then 1 else 0) boolpath
            value = path zigzag bird
        in (toRational n+1) == value   -- Successiveness can be proved inductively

prop_LeftIsFibo (NonNegative n) =
        let leftpath = replicate n 0
            n0 = denominator $ path leftpath bird
            n1 = denominator $ path (0:leftpath) bird
            n2 = denominator $ path (0:0:leftpath) bird
        in n0 + n1 == n2


findBird q n (T a [lt,rt])
        | q > a && even n || q < a && odd n  = findBird q (n+1) rt
        | q > a && odd n  || q < a && even n = findBird q (n+1) lt
        | q == a                             = True


prop_AllRationalsPresent (Positive n1) (Positive n2) = findBird (n1 % n2) 0 bird


-- Sources:
-- https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
-- http://matt.might.net/articles/quick-quickcheck/
-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
-- https://kseo.github.io/posts/2016-12-14-how-quick-check-generate-random-functions.html
-- https://stackoverflow.com/questions/16214093/how-to-generate-random-typed-functions/16220336#16220336
