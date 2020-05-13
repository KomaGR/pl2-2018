-- Γλώσσες Προγραμματισμού 2 2018
-- Ορέστης Καπαρουνάκης 03114057
-- Άσκηση 7 - Δηλωτική Σημασιολογία
import DensemSyntax
import Data.Either

-- Semantic domains

-- -- Atoms
type A = Either Int Bool
-- -- Semantic domain of values (data is bottomed automatically)
data D = Atom A | Cons (D) (D)
    deriving (Eq)

instance Show D where
    show (Atom (Left a)) = show a
    show (Atom (Right a)) = show a
    show (Cons d1 d2) = show d1 ++ ":" ++ show d2



type S = Var -> D

-- Semantic functions

semC :: C -> S -> S
semC Cskip s = s
semC (Cassign x e) s = update s x (semE e s)
semC (Cseq c1 c2) s = semC c2 (semC c1 s)
semC (Cfor e c) s
    | Atom (Left i) <- semE e s = expon i (semC c) s
    | otherwise = undefined
semC (Cif e c1 c2) s
    | Atom (Right True) <- semE e s  = semC c1 s
    | otherwise = semC c2 s
semC (Cwhile e c) s = fix bigF s
    where bigF f s | Atom (Right True) <- semE e s = f (semC c s)
                   | otherwise = s

semE :: E -> S -> D
semE Ezero s = Atom $ Left 0
semE (Esucc e) s
    | Atom (Left i) <- semE e s = Atom $ Left (i + 1)
    | otherwise = undefined
semE (Epred e) s
    | Atom (Left i) <- semE e s = Atom $ Left (i - 1)
    | otherwise = undefined
semE (Eif e0 e1 e2) s
    | semE e0 s == Atom (Right True) = semE e1 s
    | otherwise = semE e2 s
semE (Evar x) s = s x
semE Etrue s = Atom $ Right True
semE Efalse s = Atom $ Right False
semE (Elt e1 e2) s
    | Atom (Left r1) <- semE e1 s
    , Atom (Left r2) <- semE e2 s
    = Atom $ Right $ r1 < r2
    | otherwise = undefined
semE (Eeq e1 e2) s = Atom $ Right $ semE e1 s == semE e2 s
semE (Enot e) s
    | Atom (Right x) <- semE e s = Atom $ Right $ not x
    | otherwise = undefined
semE (Econs e1 e2) s = Cons (semE e1 s) (semE e2 s)
semE (Ehd e) s
    | Cons e1 _ <- semE e s = e1    -- DOES THIS WORK?
    | otherwise = undefined
semE (Etl e) s
    | Cons _ e2 <- semE e s = e2    -- DOES THIS WORK?
    | otherwise = undefined

-- auxiliary functions

expon 0 f = id
expon n f = f . expon (n-1) f

update s x n y | x == y    = n
               | otherwise = s y

fix f = f (fix f)

-- undefined = error "Dynamic Type Error"

-- Main function
main = do  input <- getContents
           let c :: C
               c = read input
           run c


-- tryouts

makeE 0 = Ezero
makeE n = Esucc (makeE (n-1))

s0 x = error ("not initialized variable " ++ x)

run c = print (semC c s0 "result")

ex0 = Cassign "result" (makeE 42)

ex1 = Cseq (Cassign "result" Ezero)
           (Cfor (makeE 6) (
              Cfor (makeE 7) (
                Cassign "result" (Esucc (Evar "result"))
              )
           ))

ex2 = Cseq (Cassign "x" (makeE 42))
      (Cseq (Cassign "result" Ezero)
            (Cwhile (Elt Ezero (Evar "x"))
              (Cseq (Cassign "x" (Epred (Evar "x")))
                    (Cassign "result" (Esucc (Evar "result"))))))


-- Sources:
-- https://courses.softlab.ntua.gr/pl2/2014b/examples/densem.hs
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- https://tuttlem.github.io/2013/01/04/recursive-data-structures-in-haskell.html