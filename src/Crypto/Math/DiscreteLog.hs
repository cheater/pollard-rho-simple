module Crypto.Math.DiscreteLog (
    solve_discrete_log
  , find_gamma
  , pollard_rho_floyd
  , check_log
  ) where


example_log = gamma
  -- solve 2^gamma = 5 (mod 1019)
  where
    n = 1018  :: Int
    n' = n + 1
    alpha = 2 :: Int
    beta = 5  :: Int
    gamma = solve_discrete_log n n' alpha beta

example_log_check = check_log alpha beta gamma n'
  -- check whether the computed logarithm is correct
  where
    n = 1018  :: Int
    n' = n + 1
    alpha = 2 :: Int
    beta = 5  :: Int
    gamma = solve_discrete_log n n' alpha beta

check_log alpha beta gamma n' = (alpha ^ gamma) `mod` n' == beta `mod` n'

solve_discrete_log :: Int -> Int -> Int -> Int -> Int
solve_discrete_log n n' alpha beta = gamma
  where
    (at, bt, ah, bh) = pollard_rho_floyd n n' alpha beta
    gamma = find_gamma n at bt ah bh

find_gamma n at bt ah bh = head $ filter pred [0..n]
  where
    b_diff = (bh - bt) `mod` n
    a_diff = (at - ah) `mod` n
    pred gamma = (b_diff * gamma) `mod` n == a_diff

pollard_rho_floyd = pollard_rho_floyd' 1 1 0 0 1 0 0

pollard_rho_floyd'
  :: Int -- i -- loop number -- FIXME: should terminate if i > n (or i >= n ?)
  -> Int -> Int -> Int -- tortoise x, a, b
  -> Int -> Int -> Int -- hare x, a, b
  -> Int -- n
  -> Int -- n' = n+1, a prime
  -> Int -> Int -- alpha, beta such that alpha^gamma == beta
  -> (Int, Int, Int, Int) -- tortoise a, tortoise b, hare a, hare b

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
