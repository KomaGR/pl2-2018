import Data.Char
import System.IO
import Text.Read
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Control.Monad.State.Strict as State

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq

-- A purely functional queue, using two lists
data Queue a = Q [a] [a]

qEmpty :: Queue a
qEmpty = Q [] []

qIsEmpty :: Queue a -> Bool
qIsEmpty (Q l r) = null l && null r

qAppendRight :: Queue a -> a -> Queue a
qAppendRight (Q l r) x = Q l (x : r)

qPopLeft :: Queue a -> (a, Queue a)
qPopLeft (Q (x : xs) r) = (x, Q xs r)
qPopLeft (Q [] []) = error "cannot pop"
qPopLeft (Q [] r) = qPopLeft (Q (reverse r) [])

qFromList = Q []

-- Pretty printing of expressions

always = True    -- False omits parentheses whenever possible

instance Show Expr where
  showsPrec p (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)


--  Main program

readExpr :: String -> Expr
readExpr s = read s :: Expr

aux_lookup :: Map.Map String Int -> String -> Int 
              -> (Map.Map String Int, Int, Int) 
aux_lookup m k n = case (Map.lookup k m) of
                      Just v -> (m, v, n)
                      Nothing -> (Map.insert k n m, n, n+1)

-- aux_lookup_2 :: IntMap.IntMap Int Int -> String -> Int 
--               -> (IntMap.IntMap String Int, Int, Int)
aux_lookup_2 m k n = case (IntMap.lookup k m) of
                      Just v -> (m, v, n)
                      Nothing -> (IntMap.insert k n m, n, n+1)
 
-- s@(m,n,c) == state@(var_map, next_number, constraint_set)
typist :: Expr -> State.State (Map.Map String Int, Int, [(Type, Type)]) Type
-- = "(Evar " ++ a ++ ")"
typist x@(Evar a) = do
                      (m,n,c) <- State.get
                      let (m1, n_prime, new_n) = aux_lookup m a n
                      State.put (m1, new_n, c)
                      State.return $ Tvar n_prime

-- = "(Eabs " ++ a ++ " " ++ typist e ++ ")"
typist x@(Eabs a e) = do
                        (m,n,c) <- State.get
                        let (oldval, m1) = Map.insertLookupWithKey f a n m
                                           where f k new old = new
                        -- let (m1, n_prime, new_n) = aux_lookup m a n
                        State.put (m1, n+1, c)
                        t2 <- typist e
                        (m2,n2,c2) <- State.get
                        let m3 = case oldval of
                                   Just val -> Map.insert a val m2
                                   Nothing -> m2
                        State.put (m3, n2, c2)
                        State.return $ (Tfun (Tvar n) (t2))

-- = "(Eapp " ++ typist e1 ++ " & " ++ typist e2 ++ ")"
typist x@(Eapp e1 e2) = do 
                          t2 <- typist e2
                          t1 <- typist e1
                          (m,n,c) <- State.get
                          let a = Tvar n        -- create new var a for t1 == t2->a
                          State.put (m,n+1,c)     
                          (m1,n1,c1) <- State.get
                          State.put (m1, n1, (t1, Tfun t2 a):c1)
                          State.return $ a
  
typist_wrapper e = State.runState (typist e) (Map.empty, 0, [])

-- Finds if t1 shows up in t2
show_up :: Type -> Type -> Bool
show_up t1 t2 | t1 == t2 = True
show_up t1 (Tfun t21 t22) = or [show_up t1 t21, show_up t1 t22]
show_up t1 (Tvar _) = False


-- Apply substitution $s in type $t
subs :: (Type, Type) -> Type -> Type
subs s@(t1, t2) t | t == t1 = t2
subs s@(t1, t2) (Tfun ct1 ct2) = Tfun (subs s ct1) (subs s ct2)
subs s@(t1, t2) t = t

-- Apply substitution $s in constraint_tuple $c
subt :: (Type, Type) -> (Type, Type) -> (Type, Type)
subt s c@(ct1,ct2) = c_prime
  where c_prime = (subs s ct1, subs s ct2)

unify :: [(Type, Type)] -> [(Type, Type)] -> Maybe [(Type, Type)]
unify [] solution = Just solution
unify ((t1, t2):c) sol | t1 == t2 = unify c sol

unify ((t1@(Tvar a), t2):c) sol | not (show_up t1 t2) = 
  unify c_prime ((t1, t2):sol) 
  where c_prime = map (subt (t1,t2)) c

unify ((t1, t2@(Tvar a)):c) sol | not (show_up t2 t1) = 
  unify c_prime ((t2, t1):sol) 
  where c_prime = map (subt (t1,t2)) c

unify ((t1@(Tfun t11 t12), t2@(Tfun t21 t22)):c) sol = 
  unify ((t11,t21):(t12,t22):c) sol

unify ((_,_):_) _ = Nothing

unify_wrapper :: (Type, (Map.Map String Int, Int, [(Type, Type)])) -> Maybe Type
unify_wrapper (t, (var_map, _, constraint_set)) = 
  case (unify constraint_set []) of
    -- Just sol -> Just (t, sol) -- subs on $t(ype) with fold
    Just sol -> Just (substitution t (qFromList sol)) -- subs on $t(ype) with fold
    Nothing -> Nothing

substitution :: Type -> Queue (Type, Type) -> Type
substitution t (Q [] []) = t
substitution t qs = substitution new_type new_qs
                    where (s, new_qs) = qPopLeft qs
                          new_type = subs s t

reassign :: Type -> State.State (IntMap.IntMap Int, Int) Type
reassign (Tfun t1 t2) = do 
                          nt1 <- reassign t1
                          nt2 <- reassign t2
                          return $ Tfun nt1 nt2
reassign (Tvar a) = do 
                      (m, n) <- State.get
                      let (new_m, n_prime, new_n) = aux_lookup_2 m a n
                      State.put (new_m, new_n)
                      return $ Tvar n_prime

reassign_wrapper :: Type -> Type
reassign_wrapper t = fst $ State.runState (reassign t) (IntMap.empty, 0)

prettify :: Maybe Type -> String
prettify Nothing = "type error"
prettify (Just t) = show $ reassign_wrapper t

-- Solve for each
solve :: String -> String
solve = prettify . unify_wrapper . typist_wrapper . readExpr

main = interact $ unlines . map solve . tail . lines

-- Sources:
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- https://eli.thegreenplace.net/2018/type-inference/
-- https://eli.thegreenplace.net/2018/unification/
-- https://mmhaskell.com/monads-5
-- https://courses.softlab.ntua.gr/pl2/2017b/examples/queue.hs