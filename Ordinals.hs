module Ordinals(Ordinal(), w, (^)) where

import Data.List
import Prelude hiding (exponent,(^))

-- Implementation for ordinals in [0,epsilon0)
-- in Cantor Normal Form (CNF)

data Term = Term { exponent :: Ordinal, multiplicity :: Integer } deriving Eq
data Ordinal = CNFPowerSum [Term] deriving Eq

zero :: Ordinal
zero = CNFPowerSum []

w :: Ordinal -> Ordinal
w a = CNFPowerSum [Term a 1]

nat :: Integer -> Ordinal
nat k 
    | k > 0  = CNFPowerSum [Term zero k]
    | k == 0 = zero
    | k < 0  = error "There are no negative ordinals"

-- Order relation
infixr 0 `orElse`
orElse :: Ordering -> Ordering -> Ordering
c1 `orElse` c2 = if (c1 /= EQ) then c1 else c2

cnf_compare :: Ordinal -> Ordinal -> Ordering
cnf_compare (CNFPowerSum ds) (CNFPowerSum es) = 
    case (ds, es) of
        ([], []) -> EQ
        ([],  _) -> LT
        (_ , []) -> GT
        (d:dt,e:et) ->
                    ((exponent d) `cnf_compare` (exponent e))
          `orElse` ((multiplicity d) `compare` (multiplicity e))
          `orElse` ((CNFPowerSum dt) `cnf_compare` (CNFPowerSum et))

-- Ordinal addition
cnf_sum :: Ordinal -> Ordinal -> Ordinal
cnf_sum alpha (CNFPowerSum []) = alpha
cnf_sum (CNFPowerSum ds) (CNFPowerSum (t:ts)) = 
    -- break off the part that will be absorbed/increased in multiplicity
    let (hi,lo) = break (\d -> (exponent d) <= (exponent t)) ds in
    -- Is the exponent of t already present (first term of lo-part)
    case lo of
        []  -> CNFPowerSum (hi ++ (t:ts))
        e:_ -> 
          if (exponent e) == (exponent t)
              then CNFPowerSum (hi ++ (t { multiplicity = (multiplicity e) + (multiplicity t) }) : ts)
              else CNFPowerSum (hi ++ (t:ts))
              
-- Ordinal multiplication
cnf_multByTerm :: Ordinal -> Term -> Ordinal
cnf_multByTerm (CNFPowerSum[]) t = 0
cnf_multByTerm (CNFPowerSum (d:ds)) (Term (CNFPowerSum[]) n) = 
    CNFPowerSum ((d { multiplicity = (multiplicity d) * n }):ds)
cnf_multByTerm (CNFPowerSum (d:ds)) (Term b n) = 
    CNFPowerSum [d { exponent = (exponent d) + b, multiplicity = n }]

cnf_mult :: Ordinal -> Ordinal -> Ordinal
cnf_mult alpha (CNFPowerSum ts) = sum [ cnf_multByTerm alpha t | t <- ts ]

-- Ordinal exponentation

-- Helpers
is_finite :: Ordinal -> Bool
is_finite (CNFPowerSum[]) = True
is_finite (CNFPowerSum [Term (CNFPowerSum[]) n]) = True
is_finite _ = False

get_finite :: Ordinal -> Integer 
get_finite (CNFPowerSum[]) = 0
get_finite (CNFPowerSum [Term (CNFPowerSum[]) n]) = n
get_finite _ = error "No finite ordinal given"

-- Finite powers via repeated squaring
-- TODO there is a faster, more explicit solution

dlog2 n p b 
    | p >= n = (p,b)
    | otherwise = dlog2 n (2*p) (b+1)

base2 n = let (p,b) = dlog2 n 1 0 in base2rec n p 
    where 
        base2rec n 0 = []
        base2rec n p 
            | n < p = 0 : base2rec n (p `div` 2)
            | otherwise = 1 : base2rec (n - p) (p `div` 2)

powerRec :: a -> a -> (a -> a -> a) -> [Int] -> a
powerRec acc p op [] = acc
powerRec acc p op (0:bits) = powerRec acc (p `op` p) op bits
powerRec acc p op (1:bits) = powerRec (acc `op` p) (p `op` p) op bits
            
finite_power :: Ordinal -> Integer -> Ordinal
finite_power a 0 = 1
finite_power a n = 
    powerRec 1 a (*) (reverse $ base2 n)    

-- Arbitrary powers
    
cnf_expByW :: Ordinal -> Ordinal -> Ordinal
cnf_expByW (CNFPowerSum []) b = 0
cnf_expByW a 0 = a
cnf_expByW (a@(CNFPowerSum ((Term b1 c1):bs))) b 
    | a == 1 = 1
    | a < w 1 && is_finite b = w (w (fromInteger ((get_finite b) - 1)))
    | a < w 1 && otherwise = w (w b)
    | otherwise = w (b1 * w b)
    
cnf_expByTerm :: Ordinal -> Term -> Ordinal
cnf_expByTerm a (Term b c) = finite_power (cnf_expByW a b) c

(^) :: Ordinal -> Ordinal -> Ordinal
a^(CNFPowerSum terms) = product [ cnf_expByTerm a t | t <- terms ]

-- Instances    
instance Num Ordinal where 
    a + b = cnf_sum a b
    a * b = cnf_mult a b
    fromInteger = nat
    
    negate = undefined
    abs = undefined
    signum = undefined

instance Ord Ordinal where
    compare = cnf_compare

-- Pretty printing

show_term :: Term -> String
show_term (Term (CNFPowerSum[]) n) = show n
show_term (Term (CNFPowerSum [Term (CNFPowerSum[]) 1]) 1) = "w"
show_term (Term (CNFPowerSum [Term (CNFPowerSum [Term (CNFPowerSum[]) 1]) 1]) 1) = "w^w"
show_term (Term (CNFPowerSum [Term (CNFPowerSum[]) n]) 1) = "w^" ++ show n
show_term (Term (CNFPowerSum [Term (CNFPowerSum[]) 1]) k) = "w*" ++ show k
show_term (Term (CNFPowerSum [Term (CNFPowerSum[]) n]) k) = "w^" ++ show n ++ "*" ++ show k
show_term (Term a 1) = "w^(" ++ show_cnf a ++ ")"
show_term (Term a n) = "w^(" ++ show_cnf a ++ ")*" ++ show n

show_cnf :: Ordinal -> String
show_cnf (CNFPowerSum[]) = "0"
show_cnf (CNFPowerSum terms) = 
    intercalate " + " [
        show_term t | t <- terms
      ]

instance Show Ordinal where
    show = show_cnf