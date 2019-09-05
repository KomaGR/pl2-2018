import Data.Char
import System.IO
import Text.Read
import qualified Data.Map.Strict as Map
import qualified Control.Monad.State.Strict as State

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq

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
                        t2 <- typist e
                        (m,n,c) <- State.get
                        let (m1, n_prime, new_n) = aux_lookup m a n
                        State.put (m1, new_n, c)
                        State.return $ (Tfun (Tvar n_prime) (t2))

-- = "(Eapp " ++ typist e1 ++ " & " ++ typist e2 ++ ")"
typist x@(Eapp e1 e2) = do 
                          t2 <- typist e2
                          (m,n,c) <- State.get
                          let a = Tvar n
                          State.put (m,n+1,c)   -- create new var a for t1 == t2->a
                          t1 <- typist e1
                          (m1,n1,c1) <- State.get
                          State.put (m1, n1, (t1, Tfun t2 a):c1)
                          State.return $ a
                        
                                

typist_wrapper e = State.runState (typist e) (Map.empty, 0, [])

-- Solve for each
solve :: String -> String
solve = show . typist_wrapper . readExpr

main = interact $ unlines . map solve . tail . lines

-- Sources:
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- https://eli.thegreenplace.net/2018/type-inference/
-- https://eli.thegreenplace.net/2018/unification/
-- https://mmhaskell.com/monads-5