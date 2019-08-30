-- Γλώσσες Προγραμματισμού 2 2018
-- Ορέστης Καπαρουνάκης 03114057
-- Άσκηση 3 - Το πουλί, το άπειρο δέντρο και οι γρήγορες δοκιμές
module Narnia where

import Pruning
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbitrarySizeTree


arbitrarySizeTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizeTree m = do
    a <- arbitrary
    n <- choose (0, m)
    as <- vectorOf n (arbitrarySizeTree (m `div` 4))
    return (T a as)

prop_HeightNaturalAndLessThanSize tree = (ht > 0) && (ht <= sizeTree tree)
                                            where
                                                ht = heightTree tree

prop_MaxTreeIsInTree tree = inTree (maxTree tree) tree

prop_EveryNodeIsInTree tree = and $ map (`inTree` tree) $ nodes tree

-- prop_CountIsNaturalAndLessThanSize :: (Tree a) -> (a -> Bool) ->
prop_CountIsNaturalAndLessThanSize tree (Blind f) = (ct > 0) && (ct <= sizeTree tree)
                                            where
                                                ct = countTree f tree
-- Φεϊλάρει για a :: (), αλλά αυτό δεν θα έπρεπε να συμβαίνει αν f :: () -> Bool

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

-- Sources:
-- https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
-- http://matt.might.net/articles/quick-quickcheck/
-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
-- https://kseo.github.io/posts/2016-12-14-how-quick-check-generate-random-functions.html
-- https://stackoverflow.com/questions/16214093/how-to-generate-random-typed-functions/16220336#16220336
