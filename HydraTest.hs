import Data.List

import Control.Applicative
import Control.Monad
import Test.QuickCheck

import Ordinals

import Hydra

-- Complexity measure for a hydra (CNF)

ord :: Hydra -> Ordinal
ord (Branch []) = 0
ord (Branch hs) = 
    let ords = sortBy (flip compare) $ map ord hs in
    sum [ w b | b <- ords ]

-- Tests
    
fail_chop_increases_heads :: HydraWithHead -> Bool
fail_chop_increases_heads (HydraWithHead hydra head) =
    let chopped = chop head hydra in
    (numHeads chopped) >= (numHeads hydra)
    
test_chop_decreasing :: HydraWithHead -> Bool
test_chop_decreasing (HydraWithHead hydra head) =
    let chopped = chop head hydra in
    (ord chopped) < (ord hydra)
    
main = do
    quickCheck test_chop_decreasing
    quickCheck fail_chop_increases_heads
    
-- Arbitrary instance for Hydras

data HydraWithHead = HydraWithHead Hydra Directions deriving Show

arbitraryHydras :: Int -> Gen [Hydra]
arbitraryHydras 0 = return []
arbitraryHydras size = do 
    firstSize <- choose (0, size-1)
    firstHydra <- arbitraryHydras firstSize
    rest <- arbitraryHydras (size - firstSize - 1)
    return $ (Branch firstHydra):rest

instance Arbitrary Hydra where
    arbitrary = sized $ \k -> Branch <$> arbitraryHydras (k + 2)
    
arbitraryHead :: Hydra -> Gen Directions
arbitraryHead (Branch []) = return []
arbitraryHead (Branch children) = do
    child <- choose (1,length children)
    rest <- arbitraryHead (children ? child)
    return (child:rest)

instance Arbitrary HydraWithHead where
    arbitrary = do 
        hydra <- arbitrary
        head <- arbitraryHead hydra
        return (HydraWithHead hydra head)