module Goodstein(
          Hereditary(..)
        , HTerm(..)
        , HSum(..)
        , hbase
        , eval
        , bump
        , bumpn
        , g
        )
        where

import Data.List (intercalate)

data Term = Term { exponent :: Integer, factor :: Integer } deriving Show

-- Helper functions
while :: (a -> Bool) -> (a -> a) -> a -> a
while cond op val 
    | cond val = while cond op (op val)
    | otherwise = val

dlog :: Integer -> Integer -> Integer
dlog b n = 
	let (p,k) = while (\(p,k) -> p <= n) (\(p,k) ->
			(p*b,k+1)
		) (1,0) in
	k - 1

baseit :: Integer -> Integer -> [Term]
baseit b 0 = []
baseit b n = 
    let d = dlog b n in
    let k = n `div` (b^d) in
    (Term d k) : baseit b (n - k*(b^d))

-- Hereditary base-b representation
    
data HTerm = HTerm { hexponent :: HSum, hfactor :: Integer } deriving Show
data HSum = HSum [HTerm] deriving Show
data Hereditary = Hereditary { base :: Integer, sums :: HSum }

baserec :: Integer -> Integer -> HSum
baserec b n = 
	let baseb = baseit b n in
	HSum $ map (\(Term exp fac) -> HTerm (baserec b exp) fac) baseb

hbase :: Integer -> Integer -> Hereditary
hbase b n = Hereditary b (baserec b n)

instance Show Hereditary where
	show (Hereditary b s) = showRec b s
		where
			showRec b (HSum []) = "0"
			showRec b (HSum terms) = intercalate " + " [
						(show b) ++ "^(" ++ showRec b exp ++ ") * " ++ (show fac)
						| (HTerm exp fac) <- terms
					]

eval :: Hereditary -> Integer
eval (Hereditary b s) = evalRec b s
	where
		evalRec b (HSum terms) = sum [ (b^(evalRec b exp)) * fac | (HTerm exp fac) <- terms ]

bump :: Hereditary -> Hereditary
bump (Hereditary b s) = Hereditary (b+1) s

bumpn :: Integer -> Integer -> Integer
bumpn b n = 
    let h = hbase b n in
    (eval (bump h)) - 1    

g :: Integer -> Integer -> Integer
g n b
    | b == 2 = n
    | otherwise = 
        let baseb = hbase (b-1) (g n (b-1)) in
        let res = eval $ bump baseb in
       	if res > 0
       		then res - 1
       		else error "Sequence terminated at 0"