module Properties (properties) where

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>))
import Crypto.Math.DiscreteLog (solve_discrete_log, check_log)

properties :: [Test]
properties = [
    testProperty "log matches" prop_guess_gamma
  ]

isPrime n = loop 2 where
  loop d
    | d*d > n        = True
    | n `rem` d == 0 = False
    | otherwise      = loop $ d+1

prop_guess_gamma n alpha gamma =
  (n > 1) ==>
  (alpha > 1) ==>
  (gamma > 1) ==>
  (isPrime n') ==>
  (isPrime alpha) ==>
  (isPrime gamma) ==>
  (check_log alpha beta guessed_gamma n')
  where
    beta = alpha ^ gamma
    n' = n+1
    guessed_gamma = solve_discrete_log n n' alpha beta
    types = (n :: Int, alpha :: Int, gamma :: Int)
