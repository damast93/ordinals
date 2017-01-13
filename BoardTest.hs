import Data.List
import Control.Applicative
import Control.Monad
import Test.QuickCheck

import Ordinals

import Board

sortDesc = sortBy (flip compare)

-- Complexity measures for boards

top_complexity :: Board -> (Int,Int)
top_complexity (Board []) = (0,0)
top_complexity (Board b) = 
    let max = maximum b in
    let nmax = length . filter (== max) $ b in
    (max,nmax)

ordinal_complexity :: Board -> Ordinal    
ordinal_complexity (Board bd) = sum [ w (fromIntegral n) | n <- sortDesc bd ]

-- Tests

fail_top_complexity_decreasing :: BoardUpdate -> Bool
fail_top_complexity_decreasing (BoardUpdate board target replacement) = 
    let newBoard = update target replacement board in
    (top_complexity newBoard) < (top_complexity board)

test_ordinal_complexity_decreasing :: BoardUpdate -> Bool
test_ordinal_complexity_decreasing (BoardUpdate board target replacement) = 
    let newBoard = update target replacement board in
    (ordinal_complexity newBoard) < (ordinal_complexity board)
    
main = do
    quickCheck test_ordinal_complexity_decreasing
    quickCheck fail_top_complexity_decreasing

-- Arbitrary instances

instance Arbitrary Board where
    arbitrary = sized $ \k -> Board <$> vectorOf (k+1) (choose (0,k))

data BoardUpdate = BoardUpdate Board Int [Int] deriving Show

instance Arbitrary BoardUpdate where
    arbitrary = do 
        board@(Board b) <- arbitrary
        target <- elements b
        replacement <- sublistOf [1..target - 1]
        return (BoardUpdate board target replacement)