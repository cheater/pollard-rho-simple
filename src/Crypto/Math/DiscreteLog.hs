module Crypto.Math.DiscreteLog (
    solve_discrete_log
  , find_gamma
  , pollard_rho_floyd
  , check_log
  , check_is_generator_in_zmodn'_mult
  , check_order_in_zmodn'_mult
  ) where

import Data.Maybe (listToMaybe)


example_log = gamma
  -- solve 2^gamma = 5 (mod 1019)
  where
    n = 1018  :: Int
    n' = n + 1
    alpha = 2 :: Int
    beta = 5  :: Int
    gamma = solve_discrete_log n n' alpha beta

example_log_check = case gamma of
  -- check whether the computed logarithm is correct
    Nothing -> False
    Just g -> check_log alpha beta g n'
  where
    n = 1018  :: Int
    n' = n + 1
    alpha = 2 :: Int
    beta = 5  :: Int
    gamma = solve_discrete_log n n' alpha beta

check_log alpha beta gamma n' = ((alpha ^ gamma) `mod` n') == (beta `mod` n')

solve_discrete_log :: (Integral n) => n -> n -> n -> n -> Maybe n
solve_discrete_log n n' alpha beta = gamma
  where
    (at, bt, ah, bh) = pollard_rho_floyd n n' alpha beta
    gamma = find_gamma n at bt ah bh


check_is_generator_in_zmodn'_mult x n' = order == (n' - 1)
  where
    order = check_order_in_zmodn'_mult x n'

check_order_in_zmodn'_mult x n' = order
  -- check order of x in the multiplicative cyclic group of integers modulo n'
  where
    xmod = x `mod` n' -- otherwise doing e.g.
    -- something check_is_generator_in_zmodn'_mult 3 3 hangs.
    cycle = xmod:(takeWhile (/= xmod) (map (\v -> (xmod * v) `mod` n') cycle))
    -- cycle: the oscillating cycle of x^k for k = 1, 2, ...
    cycle_indexed = zip [1..] cycle
    first_unit = head $ dropWhile ((/= 1).snd) $ cycle_indexed
    -- first_unit: the first time we get 1
    order = fst first_unit -- ord: the lowest k such that x^k == 1

find_gamma n at bt ah bh = safeHead $ filter pred [0..n]
  where
    b_diff = (bh - bt) `mod` n
    a_diff = (at - ah) `mod` n
    pred gamma = (b_diff * gamma) `mod` n == a_diff
    safeHead = listToMaybe

pollard_rho_floyd :: (Integral t) => t -> t -> t -> t -> (t, t, t, t)

pollard_rho_floyd = pollard_rho_floyd' 1 1 0 0 1 0 0

pollard_rho_floyd'
  :: (Integral t)
  => Int -- i -- loop number -- FIXME: terminate if i > n (or i >= n ?)
  -> t -> t -> t -- tortoise x, a, b
  -> t -> t -> t -- hare x, a, b
  -> t -- n -- FIXME: currently supports alpha coprime to n';
  -- however, this should be the order of alpha in Z_{n'}^*
  -> t -- n' = n+1, a prime
  -> t -> t -- alpha, beta such that alpha^gamma == beta
  -> (t, t, t, t) -- tortoise a, tortoise b, hare a, hare b

pollard_rho_floyd' i xt at bt xh ah bh n n' alpha beta =
  if new_xt == new_xh
    then
      (new_at, new_bt, new_ah, new_bh)
    else
      pollard_rho_floyd'
        (i+1) new_xt new_at new_bt new_xh new_ah new_bh n n' alpha beta
  where
    (new_xt, new_at, new_bt) = new_xab (xt, at, bt)
    (new_xh, new_ah, new_bh) = new_xab.new_xab $ (xh, ah, bh)

    new_xab (x, a, b) = case x `mod` 3 of
      0 -> (x*x     `mod` n', (a*2) `mod` n, (b*2) `mod` n)
      1 -> (x*alpha `mod` n', (a+1) `mod` n,  b           )
      2 -> (x*beta  `mod` n',  a           , (b+1) `mod` n)
