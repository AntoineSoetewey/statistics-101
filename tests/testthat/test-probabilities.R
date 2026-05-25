library(testthat)

# --- Continuous distributions ---

test_that("Normal: P(X <= 0) = 0.5 for standard normal", {
  expect_equal(pnorm(0, mean = 0, sd = 1), 0.5)
})

test_that("Normal: P(X <= 1.96) ~ 0.975", {
  expect_equal(round(pnorm(1.96, mean = 0, sd = 1), 3), 0.975)
})

test_that("Normal: upper tail P(X > x) = 1 - P(X <= x)", {
  x <- 1.5; mu <- 2; sd <- 3
  expect_equal(pnorm(x, mu, sd, lower.tail = FALSE), 1 - pnorm(x, mu, sd))
})

test_that("Normal: interval P(a <= X <= b) = P(X <= b) - P(X <= a)", {
  a <- -1; b <- 1
  expect_equal(pnorm(b) - pnorm(a), pnorm(b) - pnorm(a))
  expect_equal(round(pnorm(b) - pnorm(a), 4), 0.6827)
})

test_that("Exponential: P(X <= x) formula", {
  x <- 2; rate <- 0.5
  expect_equal(pexp(x, rate), 1 - exp(-rate * x))
})

test_that("Chi-square: CDF is consistent with density integral", {
  df <- 5
  # P(X <= 5) via pchisq and numerical integration should be close
  expect_equal(round(pchisq(5, df), 4), round(integrate(function(x) dchisq(x, df), 0, 5)$value, 4))
})

test_that("Student t: symmetric around 0", {
  df <- 10
  expect_equal(pt(-1, df), 1 - pt(1, df))
})

test_that("Beta: CDF at x=1 equals 1", {
  expect_equal(pbeta(1, shape1 = 2, shape2 = 3), 1)
})

test_that("Beta: CDF at x=0 equals 0", {
  expect_equal(pbeta(0, shape1 = 2, shape2 = 3), 0)
})

test_that("Gamma: CDF is consistent with density integral", {
  shape <- 2; rate <- 1
  expect_equal(
    round(pgamma(3, shape, rate), 4),
    round(integrate(function(x) dgamma(x, shape, rate), 0, 3)$value, 4)
  )
})

test_that("Weibull: CDF at x=0 equals 0", {
  expect_equal(pweibull(0, shape = 2, scale = 1), 0)
})

test_that("Log-Normal: P(X <= exp(mu)) = 0.5", {
  mu <- 1; sigma <- 1
  expect_equal(plnorm(exp(mu), meanlog = mu, sdlog = sigma), 0.5)
})

test_that("Cauchy: CDF at median equals 0.5", {
  expect_equal(pcauchy(5, location = 5, scale = 1), 0.5)
})

test_that("Logistic: CDF at location equals 0.5", {
  expect_equal(plogis(3, location = 3, scale = 1), 0.5)
})

test_that("Fisher: CDF at x=0 equals 0", {
  expect_equal(pf(0, df1 = 5, df2 = 10), 0)
})

# --- Discrete distributions ---

test_that("Binomial: lower tail P(X <= x) matches sum of PMF", {
  n <- 10; p <- 0.4; x <- 4
  expect_equal(pbinom(x, n, p), sum(dbinom(0:x, n, p)))
})

test_that("Binomial: upper tail P(X > x) = 1 - P(X <= x)", {
  n <- 10; p <- 0.3; x <- 5
  expect_equal(pbinom(x, n, p, lower.tail = FALSE), 1 - pbinom(x, n, p))
})

test_that("Binomial: interval P(a <= X <= b) uses a-1 adjustment", {
  n <- 10; p <- 0.5; a <- 3; b <- 7
  formula_result <- pbinom(b, n, p) - pbinom(a - 1, n, p)
  direct_sum <- sum(dbinom(a:b, n, p))
  expect_equal(formula_result, direct_sum)
})

test_that("Poisson: lower tail matches sum of PMF", {
  lambda <- 3; x <- 4
  expect_equal(ppois(x, lambda), sum(dpois(0:x, lambda)))
})

test_that("Poisson: interval uses a-1 adjustment", {
  lambda <- 5; a <- 2; b <- 6
  formula_result <- ppois(b, lambda) - ppois(a - 1, lambda)
  direct_sum <- sum(dpois(a:b, lambda))
  expect_equal(formula_result, direct_sum)
})

test_that("Geometric I (failures): P(X <= x) = pgeom(x, p)", {
  p <- 0.3; x <- 3
  # X = number of failures before first success; X ~ Geom(p) in R
  expect_equal(pgeom(x, p), sum(dgeom(0:x, p)))
})

test_that("Geometric II (trials): P(X <= x) = pgeom(x-1, p)", {
  p <- 0.3; x <- 4
  # X = number of trials until first success; X = Y + 1 where Y ~ Geom(p)
  formula_result <- pgeom(x - 1, p)
  direct_sum <- sum(dgeom(0:(x - 1), p))
  expect_equal(formula_result, direct_sum)
})

test_that("Geometric II: interval P(a <= X <= b) uses pgeom(b-1, p) - pgeom(a-2, p)", {
  p <- 0.4; a <- 2; b <- 5
  formula_result <- pgeom(b - 1, p) - pgeom(a - 2, p)
  direct_sum <- sum(dgeom((a - 1):(b - 1), p))
  expect_equal(formula_result, direct_sum)
})

test_that("Negative Binomial I (failures): P(X <= x) = pnbinom(x, r, p)", {
  r <- 3; p <- 0.4; x <- 5
  expect_equal(pnbinom(x, r, p), sum(dnbinom(0:x, r, p)))
})

test_that("Negative Binomial II (trials): P(X <= x) = pnbinom(x-r, r, p)", {
  r <- 3; p <- 0.4; x <- 8
  # X = total trials; X = Y + r where Y ~ NB(r, p) in R
  formula_result <- pnbinom(x - r, r, p)
  direct_sum <- sum(dnbinom(0:(x - r), r, p))
  expect_equal(formula_result, direct_sum)
})

test_that("Negative Binomial II: interval uses pnbinom(b-r, r, p) - pnbinom(a-r-1, r, p)", {
  r <- 2; p <- 0.5; a <- 3; b <- 7
  formula_result <- pnbinom(b - r, r, p) - pnbinom(a - r - 1, r, p)
  direct_sum <- sum(dnbinom((a - r):(b - r), r, p))
  expect_equal(formula_result, direct_sum)
})

test_that("Hypergeometric: lower tail matches sum of PMF", {
  N <- 20; K <- 7; n <- 5; x <- 3
  expect_equal(phyper(x, K, N - K, n), sum(dhyper(0:x, K, N - K, n)))
})

test_that("Hypergeometric: interval uses a-1 adjustment", {
  N <- 20; K <- 7; n <- 5; a <- 1; b <- 3
  formula_result <- phyper(b, K, N - K, n) - phyper(a - 1, K, N - K, n)
  direct_sum <- sum(dhyper(a:b, K, N - K, n))
  expect_equal(formula_result, direct_sum)
})

# --- Distribution parameter boundary checks ---

test_that("Binomial probabilities sum to 1", {
  n <- 8; p <- 0.6
  expect_equal(round(sum(dbinom(0:n, n, p)), 10), 1)
})

test_that("Poisson probabilities are non-negative", {
  lambda <- 4
  expect_true(all(dpois(0:20, lambda) >= 0))
})

test_that("Normal PDF integrates to 1", {
  result <- integrate(function(x) dnorm(x, mean = 0, sd = 1), -Inf, Inf)$value
  expect_equal(round(result, 6), 1)
})
