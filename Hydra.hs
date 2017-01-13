module Hydra where

import Data.List

-- List helpers

dropNth :: Int -> [a] -> [a]
dropNth n list = take (n-1) list ++ (drop (n) list)

update :: Int -> (a -> a) -> [a] -> [a]
update n f list = [ if i == n then f a else a | (a,i) <- zip list [1..] ]

(?) :: [a] -> Int -> a
list ? n = list !! (n - 1)

-- Hydra

data Hydra = Branch [Hydra] deriving Show

isHead :: Hydra -> Bool
isHead (Branch l) = length l == 0

numHeads :: Hydra -> Int
numHeads (Branch []) = 1
numHeads (Branch hs) = sum [ numHeads h | h <- hs ]

type Directions = [Int]

chop :: Directions -> Hydra -> Hydra
chop [] hydra
	| isHead hydra = error "It's dead already"
	| otherwise = error "Can only chop off heads"

chop [i] (Branch hydras) 
	| isHead (hydras ? i) = Branch (dropNth i hydras)
	| otherwise = error "Can only chop off heads"

chop [i,j] (Branch hydras) = 
	let parent = hydras ? i in 
	let newParent = chop [j] parent in
	Branch (
		dropNth i hydras ++ replicate 3 newParent
		)

chop (l:ls) (Branch hydras) = Branch (
		update l (chop ls) hydras
	) 

-- Get some simple hydra

h :: Hydra
h = Branch [
		Branch [
			Branch [
				Branch [],
				Branch []
			]
		]
	]

-- Cut it down
    
selectHead :: Hydra -> Directions
selectHead (Branch []) = []
selectHead (Branch (h:hs)) = 1 : selectHead h

cutDown :: Int -> Hydra -> Int
cutDown acc h 
	| isHead h  = acc
	| otherwise = cutDown (acc+1) $ chop (selectHead h) h