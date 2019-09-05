import Data.Char
import System.IO
import Text.Read
import qualified Data.Map.Lazy as Map
import qualified Control.Monad.State.Lazy as State

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

aux_lookup m k n = case (Map.lookup k m) of
                      Just v -> (m, v, n)
                      Nothing -> (Map.insert k n m, n, n+1)
 
-- s@(m,n,c) == state@(var_map, next_number, constraint_set)
typist :: (Map.Map String Int, Int, [(Type, Type)]) 
          -> Expr 
          -> ((Map.Map String Int, Int, [(Type, Type)]), Type)
-- = "(Evar " ++ a ++ ")"
typist s@(m,n,c) x@(Evar a) = ((m1,new_n,c), Tvar n)
                      where (m1, n, new_n) = aux_lookup m a n

-- = "(Eabs " ++ a ++ " " ++ typist e ++ ")"
typist s@(m,n,c) x@(Eabs a e) = ((m2,n2,c2), Tfun (Tvar n) (t2))
                      where (m1, n, new_n) = aux_lookup m a n
                            ((m2,n2,c2), t2) = typist (m1,new_n,c) e

-- = "(Eapp " ++ typist e1 ++ " & " ++ typist e2 ++ ")"
typist s@(m,n,c) x@(Eapp e1 e2) = ((m2,n2,c2), t1)
                          where ((m1,n1,c1), t1) = typist (m,n,c) e1
                                ((m2,n2,c2), t2) = typist (m1,n1,c1) e2
                                

-- Solve for each
solve :: String -> String
solve = show . typist (Map.empty, 0, []) . readExpr

main = interact $ unlines . map solve . tail . lines

-- Sources:
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- https://eli.thegreenplace.net/2018/type-inference/
-- https://eli.thegreenplace.net/2018/unification/
-- https://mmhaskell.com/monads-5