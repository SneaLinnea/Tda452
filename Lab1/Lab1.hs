import Test.QuickCheck
{-
  Part 1
  Power n k takes one step and then uses n (k-1), which takes k steps,
  from 0 to k-1, so totally the computation takes k+1 steps.
-}
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

--Part 2
power1 :: Integer -> Integer -> Integer
power1 n k |k < 0 = error "power1: negative argument"
power1 n k = product (replicate (fromInteger k) n)

--Part 3
power2 :: Integer -> Integer -> Integer
power2 n k
  |k < 0 = error "power2: negative argument"
  |k == 0 = 1
  |even k = power2 (n*n) (div k 2)
  |odd k = n * power2 n (k-1)

{-
  Part 4
  A. I have chosen the test cases k = 0, 1, 2. Because I would like to test
  all cases in each function; for power we would like to test
  k=0 and a positive k.
    For power1 we want to test a positive k > 1, to be sure that the inner
  function replicate works as it should. (if k=0 we create an empty list
  but the product of an empty list is 1 in haskell so that works out well).
    For power2 we want to try k=0, an even number k and an odd
  number k, to evaluate each of the lines.
    The number n does not have any constraints in the functions so it should
  not matter which values we choose for n. (So I choose n=2)
-}

--Part 4 B.
prop_powers :: Integer -> Integer -> Bool
prop_powers n k
  |power n k == power1 n k && power n k == power2 n k
  && power1 n k == power2 n k = True
  |otherwise = False

--Part 4 C.
testPowers :: Bool
testPowers = and (zipWith prop_powers nList kList)
  where
    nList = (replicate 3 2)
    kList = [0..2]

--Part 4 D. QuickCheck tests negative values and (obviously) gets an error;
--so I create a prop_powers where negative k valuates to True
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k
  |k < 0 = True
  |power n k == power1 n k && power n k == power2 n k
  && power1 n k == power2 n k = True
  |otherwise = False
