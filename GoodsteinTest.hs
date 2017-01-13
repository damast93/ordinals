import Data.List

import Control.Applicative
import Control.Monad
import Test.QuickCheck

import Ordinals

import Goodstein

-- Ordinals

ord :: Hereditary -> Ordinal
ord (Hereditary b s) = ordRec s
	where
		ordRec (HSum terms) = sum [ (w (ordRec exp)) * (fromInteger fac) | (HTerm exp fac) <- terms ]

ordn :: Integer -> Integer -> Ordinal
ordn b n = ord (hbase b n)
        
-- Tests

test_hereditary_works :: Integer -> Integer -> Bool
test_hereditary_works n b = 
    (b < 2) || (eval (hbase b n) == n)

test_example_1 :: Bool
test_example_1 = (g 4 3) == 26

test_example_2 :: Bool
test_example_2 = (ordn 3 26) == ((w 2)*2 + (w 1)*2 + 2)

test_ordn_decreasing :: TestPair -> Bool
test_ordn_decreasing (TestPair b n) = ordn (b+1) (bumpn b n) < ordn b n

main = do
    quickCheck test_ordn_decreasing
    quickCheck test_example_1
    quickCheck test_example_2
    
-- Arbitrary instances

data TestPair = TestPair Integer Integer deriving Show

instance Arbitrary TestPair where
    arbitrary = do 
        b <- choose (2,5)
        n <- choose (1,10000)
        return $ TestPair b n