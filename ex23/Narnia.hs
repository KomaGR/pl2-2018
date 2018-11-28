-- Γλώσσες Προγραμματισμού 2 2018
-- Ορέστης Καπαρουνάκης 03114057
-- Άσκηση 3 - Το πουλί, το άπειρο δέντρο και οι γρήγορες δοκιμές
module Narnia where

import Pruning
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Tree a)

-- Sources:
-- https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
