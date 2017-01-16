import Ordinals

import Control.Monad
import Control.Applicative
import Test.QuickCheck

import Prelude hiding ((^))
import qualified Prelude as P

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
    
test_w_monotonic :: Ordinal -> Ordinal -> Bool
test_w_monotonic a b = (a < b) == (w a < w b)

test_wtransfinite :: NonNegative Integer -> Bool
test_wtransfinite (NonNegative n) = (fromInteger n) < (w 1)

test_w_is_exponentiation :: Ordinal -> Bool
test_w_is_exponentiation a = (w a) == ((w 1)^a)

test_powerlaw_1 :: Ordinal -> Ordinal -> Ordinal -> Bool
test_powerlaw_1 a b c = a^(b+c) == (a^b)*(a^c)

test_powerlaw_2 :: Ordinal -> Ordinal -> Ordinal -> Bool
test_powerlaw_2 a b c = (a^b)^c == a^(b*c)

test_exp_zero :: Ordinal -> Bool
test_exp_zero a = a^0 == 1

test_exp_one :: Ordinal -> Bool
test_exp_one a = a^1 == a

test_power_one :: Ordinal -> Bool
test_power_one a = 1^a == 1

test_power_zero :: Ordinal -> Bool
test_power_zero a = (a > 0) `implies` (0^a == 0)

test_squaring :: Ordinal -> Bool
test_squaring a = a^2 == a*a

test_exp_strictness :: Ordinal -> Ordinal -> Ordinal -> Bool
test_exp_strictness a b c = 
    (a < b && c > 1) `implies` ((c ^ a < c ^ b) && (a ^ c <= b ^ c))

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
fail_multiplication_strictness a b c = (a < b && c > 0) `implies` (a * c < b * c)

fail_exp_strictness :: Ordinal -> Ordinal -> Ordinal -> Bool
fail_exp_strictness a b c = (a < b && c > 1) `implies` (a ^ c < b ^ c)

fail_powerlaw_base :: Ordinal -> Ordinal -> Ordinal -> Bool
fail_powerlaw_base a b c = (a*b)^c == (a^c)*(b^c)

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
    , "Monotonicity of w" <?> test_w_monotonic
    , "Exponential law for w" <?> test_exponential_law
    , "w well-defined exponential" <?> test_w_is_exponentiation
    , "Power law 1" <?> test_powerlaw_1
    , "Power law 2" <?> test_powerlaw_2
    , "a^0 = 1" <?> test_exp_zero
    , "a^1 = a" <?> test_exp_one
    , "1^a = 1" <?> test_power_one
    , "0^a = 0 (0<a)" <?> test_power_zero
    , "Squaring" <?> test_squaring
    , "Monotonicity of exponentiation" <?> test_exp_strictness 
    , "w is transfinite" <?> test_wtransfinite
    
    , "Example 1" <?> ((w 2 + w 3) == w 3)
    , "Example 2" <?> (((w 3 + w 2) * 3) == ((w 3) * 3 + w 2))
    , "Example 3" <?> ((2 * w 1) == (w 1))
    , "Example 4" <?> (((w 3 + w 2)*(w 1)) == (w 4))
    , "Example 5" <?> (((w 1 + 1)*2) == ((w 1)*2 + 1))
    , "Example 6" <?> (((w 2)*5 + (w 3)*2) == (w 3)*2)
    , "Example 7" <?> ((2^(w 1)) == w 1)
    , "Example 8" <?> ((((w 1)*2)^(w 1)) == ((w 1)^(w 1))*2)
    , "Example 9" <?> ((w 2 * 2)^(w 1) == (w (w 1)))
  ]

finite_cases :: [TestCase]
finite_cases = [
       "Addition is ordinary addition (Finite)" <?> \(Finite a na) (Finite b nb) -> (a+b) == fromInteger (na + nb)
    ,  "Multiplication is ordinary Multiplication (Finite)" <?> \(Finite a na) (Finite b nb) -> (a*b) == fromInteger (na * nb)
    ,  "Exponentiation is ordinary exponentiation (Finite)" <?> \(Finite a na) (Finite b nb) -> (a^b) == fromInteger (na P.^ nb)
    ,  "Commutativity of addition (Finite)" <?> \(Finite a _) (Finite b _) -> fail_addition_commutative a b
    , "Commutativity of multiplication (Finite)" <?> \(Finite a _) (Finite b _) -> fail_multiplication_commutative a b
    , "Left distributivity (Finite)" <?> \(Finite a _) (Finite b _) (Finite c _) -> fail_distributivity a b c
    , "Strict monotonicity of addition (Finite)" <?> \(Finite a _) (Finite b _) (Finite c _) -> fail_addition_strictness a b c
    , "Strict monotonicity of multiplication (Finite)" <?> \(Finite a _) (Finite b _) (Finite c _) -> fail_multiplication_strictness a b c
    , "Strict monotonicity of exponentiation (Finite)" <?> \(Finite a _) (Finite b _) (Finite c _) -> fail_exp_strictness a b c
    , "Power law in the base (Finite)" <?> \(Finite a _) (Finite b _) (Finite c _) -> fail_powerlaw_base a b c
  ]

fail_cases :: [TestCase]
fail_cases = [
      "Commutativity of addition" <?> fail_addition_commutative
    , "Commutativity of multiplication" <?> fail_multiplication_commutative
    , "Left distributivity" <?> fail_distributivity
    , "Strict monotonicity of addition" <?> fail_addition_strictness
    , "Strict monotonicity of multiplication" <?> fail_multiplication_strictness
    , "Strict monotonicity of exponentiation" <?> fail_exp_strictness
    , "Power law in the base" <?> fail_powerlaw_base
  ]
  
main :: IO ()
main = do
    putStrLn "------------------- Tests: -------------------"
    sequence_ [ putStr (s ++ ": ") >> check p | (TestCase s p) <- test_cases ]
    sequence_ [ putStr (s ++ ": ") >> check p | (TestCase s p) <- finite_cases ]
    putStrLn "------------------- Fails: -------------------"
    sequence_ [ putStr (s ++ ": ") >> check p | (TestCase s p) <- fail_cases ]
  where
    check p = quickCheckWith args p
    args = stdArgs { maxSize = 30, maxSuccess = 200 }

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

-- Test cases for finite ordinals
data Finite = Finite Ordinal Integer deriving Show

instance Arbitrary Finite where
    arbitrary = do
        k <- choose (0,30)
        return $ Finite (fromInteger k) k 

-- Helpers
implies a b = (not a) || b