module Properties (properties) where

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), (===), verbose, once, within)
import Crypto.Math.DiscreteLog (
    solve_discrete_log
  , check_is_generator_in_zmodm_mult
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

check_log_qc alpha beta gamma m = ((alpha ^ gamma) `mod` m) === (beta `mod` m)

prop_guess_gamma n alpha gamma =
  verbose.(within (2 * 1000 * 1000)) $
  (n > 1) ==>
  (alpha > 1) ==>
  (alpha < m) ==>
  (gamma > 1) ==>
  (isPrime m) ==>
  (check_is_generator_in_zmodm_mult alpha m) ==>
--  ((alpha `mod` m) > 1) ==>
  (isJust guessed_gamma) ==>
  (check_log_qc alpha beta (fromJust guessed_gamma) m)
  where
    beta = (alpha ^ gamma) `mod` m
    m = n+1
    guessed_gamma = solve_discrete_log n m alpha beta
    types = (n :: Integer, alpha :: Integer, gamma :: Integer)

