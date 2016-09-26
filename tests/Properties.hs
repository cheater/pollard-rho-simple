module Properties (properties) where

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), verbose, once, within)
import Crypto.Math.DiscreteLog (
    solve_discrete_log
  , check_log
  , check_is_generator_in_zmodn'_mult
  )
import Data.Maybe (isJust, fromJust)

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
  verbose.(within (2 * 1000 * 1000)) $
  (n > 1) ==>
  (alpha > 1) ==>
  (alpha < n') ==>
  (gamma > 1) ==>
  (isPrime n') ==>
  (check_is_generator_in_zmodn'_mult alpha n') ==>
--  ((alpha `mod` n') > 1) ==>
  (isJust guessed_gamma) ==>
  (check_log alpha beta (fromJust guessed_gamma) n')
  where
    beta = (alpha ^ gamma) `mod` n'
    n' = n+1
    guessed_gamma = solve_discrete_log n n' alpha beta
    types = (n :: Integer, alpha :: Integer, gamma :: Integer)

