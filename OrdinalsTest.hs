import Ordinals

import Control.Monad
import Control.Applicative
import Test.QuickCheck

-- Tests

test_zero_minimal :: Ordinal -> Bool
test_zero_minimal a = 0 <= a

test_unit_zero :: Ordinal -> Bool
test_unit_zero a = ((a + 0) == (0 + a)) && ((a + 0) == a)

test_unit_one :: Ordinal -> Bool
test_unit_one a = ((a * 1) == (1 * a)) && ((a*1) == a)

test_plus_associative :: Ordinal -> Ordinal -> Ordinal -> Bool
test_plus_associative a b c = ((a + b) + c) == (a + (b + c))

test_trichotomy :: Ordinal -> Ordinal -> Bool
test_trichotomy a b = (length $ filter id [a < b, a > b, a == b]) == 1

test_mult_associative :: Ordinal -> Ordinal -> Ordinal -> Bool
test_mult_associative a b c = ((a * b) * c) == (a * (b * c))

test_distributive :: Ordinal -> Ordinal -> Ordinal -> Bool
test_distributive a b c = (a * (b + c)) == (a * b + a * c)

test_addition_monotonic :: Ordinal -> Ordinal -> Ordinal -> Bool
test_addition_monotonic a b c =  
    (a < b) `implies` ((c + a < c + b) && (a + c <= b + c))
    
test_multiplication_monotonic :: Ordinal -> Ordinal -> Ordinal -> Bool
test_multiplication_monotonic a b c =  
    (a < b && c > 0) `implies` ((c * a < c * b) && (a * c <= b * c))

test_exponential_law :: Ordinal -> Ordinal -> Bool
test_exponential_law a b = (w a) * (w b) == (w (a+b)) 
    
test_exp_monotonic :: Ordinal -> Ordinal -> Bool
test_exp_monotonic a b = (a < b) == (w a < w b)

test_wtransfinite :: NonNegative Integer -> Bool
test_wtransfinite (NonNegative n) = (fromInteger n) < (w 1)

-- Tests that should fail

fail_addition_commutative :: Ordinal -> Ordinal -> Bool
fail_addition_commutative a b = (a + b) == (b + a)

fail_multiplication_commutative :: Ordinal -> Ordinal -> Bool
fail_multiplication_commutative a b = (a * b) == (b * a)

fail_distributivity :: Ordinal -> Ordinal -> Ordinal -> Bool
fail_distributivity a b c = ((a+b)*c) == (a*c + b*c)

fail_addition_strictness :: Ordinal -> Ordinal -> Ordinal -> Bool
fail_addition_strictness a b c = (a < b) `implies` (a + c < b + c)

fail_multiplication_strictness :: Ordinal -> Ordinal -> Ordinal -> Bool
fail_multiplication_strictness a b c = (a < b) `implies` (a * c < b * c)

-- Mini testing framework

data TestCase = TestCase String Property

(<?>) :: Testable p => String -> p -> TestCase
(<?>) s p = TestCase s (property p)

test_cases :: [TestCase]
test_cases = [
      "Minimality of 0" <?> test_zero_minimal
    , "Unitality of 0" <?> test_unit_zero
    , "Unitality of 1" <?> test_unit_one
    , "Associativity of +" <?> test_plus_associative
    , "Associativity of *" <?> test_mult_associative
    , "Right distributivity" <?> test_distributive
    , "Trichotomy" <?> test_trichotomy
    , "Monotonicity of addition" <?> test_addition_monotonic
    , "Monotonicity of multiplication" <?> test_multiplication_monotonic
    , "Monotonicity of exponentiation" <?> test_exp_monotonic
    , "Exponential law" <?> test_exponential_law
    , "w is transfinite" <?> test_wtransfinite
    
    , "Example 1" <?> ((w 2 + w 3) == w 3)
    , "Example 2" <?> (((w 3 + w 2) * 3) == ((w 3) * 3 + w 2))
    , "Example 3" <?> ((2 * w 1) == (w 1))
    , "Example 4" <?> (((w 3 + w 2)*(w 1)) == (w 4))
    , "Example 5" <?> (((w 1 + 1)*2) == ((w 1)*2 + 1))
    , "Example 6" <?> (((w 2)*5 + (w 3)*2) == (w 3)*2)
  ]

fail_cases :: [TestCase]
fail_cases = [
      "Commutativity of addition" <?> fail_addition_commutative
    , "Commutativity of multiplication" <?> fail_multiplication_commutative
    , "Left distributivity" <?> fail_distributivity
    , "Strict monotonicity of addition" <?> fail_addition_strictness
    , "Strict monotonicity of multiplication" <?> fail_multiplication_strictness
  ]
  
main :: IO ()
main = do
    putStrLn "Tests: "
    sequence_ [ putStr (s ++ ": ") >> check p | (TestCase s p) <- test_cases ]
    putStrLn "Fails: "
    sequence_ [ putStr (s ++ ": ") >> check p | (TestCase s p) <- fail_cases ]
  where
    check p = quickCheckWith args p
    args = stdArgs { maxSize = 50, maxSuccess = 500 }

-- Generate Arbitrary instance for Ordinal arithmetic

data Arithmetic = 
      Nat Integer
    | Sum Arithmetic Arithmetic
    | Mul Arithmetic Arithmetic
    | WTo Arithmetic
    deriving (Eq,Show)

natural :: Gen Integer
natural = choose (0,10)
    
arbitraryArithmeticSized :: Int -> Gen Arithmetic
arbitraryArithmeticSized 0 = Nat <$> natural
arbitraryArithmeticSized n = 
    frequency [
        (2,natcase),
        (4,sumcase n),
        (2, mulcase n),
        (3,wcase n)
      ]
    where
      natcase = Nat <$> natural
      sumcase n = do
          a <- arbitraryArithmeticSized (n `div` 2)
          b <- arbitraryArithmeticSized (n `div` 2)
          return $ Sum a b
      mulcase n = do
          a <- arbitraryArithmeticSized (n `div` 2)
          b <- arbitraryArithmeticSized (n `div` 2)
          return $ Mul a b
      wcase n = WTo <$> arbitraryArithmeticSized (n - 1)

instance Arbitrary Arithmetic where
    arbitrary = sized arbitraryArithmeticSized

eval :: Arithmetic -> Ordinal
eval (Nat k) = fromInteger k
eval (Sum a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)
eval (WTo e) = w (eval e)
    
-- Use Arbitrary terms to generate arbitrary ordinals
instance Arbitrary Ordinal where
    arbitrary = eval <$> arbitrary
    
implies a b = (not a) || b