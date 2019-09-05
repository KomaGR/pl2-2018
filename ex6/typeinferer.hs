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

aux_lookup m a = case (Map.lookup a m) of 
                  Just number -> (m, number)
                  Nothing -> (Map.insert a number m, number)
                                    where number = Map.size m

typist :: Map.Map String Int -> Expr -> (Map.Map String Int, Type)
-- = "(Evar " ++ a ++ ")"
typist m x@(Evar a) = Tvar number
                      where (_, number) = aux_lookup m a            

-- = "(Eabs " ++ a ++ " " ++ typist e ++ ")" 
typist m x@(Eabs a e) = Tfun (Tvar number) (typist m1 e)
                      where (m1, number) = aux_lookup m a

-- = "(Eapp " ++ typist e1 ++ " & " ++ typist e2 ++ ")"
typist m x@(Eapp e1 e2) = Tvar t1
                          where (m1, t1) = typist m e1
                                (m2, t2) = typist m1 e1

-- Solve for each
solve :: String -> String
solve = show . typist Map.empty . readExpr

main = interact $ unlines . map solve . tail . lines

-- Sources:
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- https://eli.thegreenplace.net/2018/type-inference/
-- https://eli.thegreenplace.net/2018/unification/
-- https://mmhaskell.com/monads-5