library(testthat)

# Tests for the app-specific probability reparameterizations used in server.R.
# These are non-trivial choices made by the app: Geometric II shifts x by -1,
# Negative Binomial II shifts x by -r, and interval formulas use a-1 (or a-2,
# a-r-1) adjustments to convert inclusive bounds into R's p-function arguments.

# --- Geometric II: X = number of trials until first success ---
# The app uses pgeom(x - 1, p) because R's pgeom counts failures, not trials.

test_that("Geometric II lower tail: pgeom(x-1, p) is correct for X = trials", {
  p <- 0.3; x <- 4
  app_result  <- pgeom(x - 1, prob = p)
  manual_result <- sum(dgeom(0:(x - 1), prob = p))
  expect_equal(app_result, manual_result)
})

test_that("Geometric II upper tail: pgeom(x-1, p, lower.tail=FALSE) is correct", {
  p <- 0.4; x <- 3
  # P(X > x) where X = trials = P(Y >= x) where Y = failures; manual sum starts at y = x
  app_result  <- pgeom(x - 1, prob = p, lower.tail = FALSE)
  manual_result <- sum(dgeom(x:200, prob = p))
  expect_equal(round(app_result, 8), round(manual_result, 8))
})

test_that("Geometric II interval: pgeom(b-1, p) - pgeom(a-2, p) is correct", {
  p <- 0.4; a <- 2; b <- 5
  # P(a <= X <= b) where X = trials = P(a-1 <= Y <= b-1) where Y = failures
  app_result  <- pgeom(b - 1, prob = p) - pgeom(a - 2, prob = p)
  manual_result <- sum(dgeom((a - 1):(b - 1), prob = p))
  expect_equal(app_result, manual_result)
})

# --- Negative Binomial II: X = total trials until r-th success ---
# The app uses pnbinom(x - r, r, p) because R's pnbinom counts failures, not trials.

test_that("NB II lower tail: pnbinom(x-r, r, p) is correct for X = total trials", {
  r <- 3; p <- 0.4; x <- 8
  app_result  <- pnbinom(x - r, size = r, prob = p)
  manual_result <- sum(dnbinom(0:(x - r), size = r, prob = p))
  expect_equal(app_result, manual_result)
})

test_that("NB II upper tail: pnbinom(x-r, r, p, lower.tail=FALSE) is correct", {
  r <- 2; p <- 0.5; x <- 6
  # P(X > x) where X = total trials = P(Y >= x-r+1) where Y = failures
  app_result  <- pnbinom(x - r, size = r, prob = p, lower.tail = FALSE)
  manual_result <- sum(dnbinom((x - r + 1):200, size = r, prob = p))
  expect_equal(round(app_result, 8), round(manual_result, 8))
})

test_that("NB II interval: pnbinom(b-r, r, p) - pnbinom(a-1-r, r, p) is correct", {
  r <- 2; p <- 0.5; a <- 3; b <- 7
  app_result  <- pnbinom(b - r, size = r, prob = p) - pnbinom(a - 1 - r, size = r, prob = p)
  manual_result <- sum(dnbinom((a - r):(b - r), size = r, prob = p))
  expect_equal(app_result, manual_result)
})

# --- Geometric I and NB I: a-1 adjustment for interval probabilities ---
# Discrete distributions use P(a <= X <= b) = F(b) - F(a-1), not F(b) - F(a).

test_that("Geometric I interval: pgeom(b, p) - pgeom(a-1, p) is correct", {
  p <- 0.3; a <- 2; b <- 6
  app_result  <- pgeom(b, prob = p) - pgeom(a - 1, prob = p)
  manual_result <- sum(dgeom(a:b, prob = p))
  expect_equal(app_result, manual_result)
})

test_that("NB I interval: pnbinom(b, r, p) - pnbinom(a-1, r, p) is correct", {
  r <- 3; p <- 0.4; a <- 2; b <- 8
  app_result  <- pnbinom(b, size = r, prob = p) - pnbinom(a - 1, size = r, prob = p)
  manual_result <- sum(dnbinom(a:b, size = r, prob = p))
  expect_equal(app_result, manual_result)
})

# --- Hypergeometric: parameter mapping N, M, n → m, n, k in R's phyper ---
# The app calls phyper(x, m = M, n = N - M, k = n) where N = population size,
# M = number of successes in population, n = sample size (app notation).

test_that("Hypergeometric lower tail: phyper with app's parameter mapping is correct", {
  N <- 20; M <- 7; n <- 5; x <- 3
  app_result  <- phyper(x, m = M, n = N - M, k = n)
  manual_result <- sum(dhyper(0:x, m = M, n = N - M, k = n))
  expect_equal(app_result, manual_result)
})

test_that("Hypergeometric interval: a-1 adjustment is correct", {
  N <- 20; M <- 7; n <- 5; a <- 1; b <- 3
  app_result  <- phyper(b, m = M, n = N - M, k = n) - phyper(a - 1, m = M, n = N - M, k = n)
  manual_result <- sum(dhyper(a:b, m = M, n = N - M, k = n))
  expect_equal(app_result, manual_result)
})

# --- Binomial and Poisson: a-1 interval adjustment (shared pattern) ---

test_that("Binomial interval: pbinom(b) - pbinom(a-1) is correct", {
  n <- 10; p <- 0.5; a <- 3; b <- 7
  app_result  <- pbinom(b, n, p) - pbinom(a - 1, n, p)
  manual_result <- sum(dbinom(a:b, n, p))
  expect_equal(app_result, manual_result)
})

test_that("Poisson interval: ppois(b) - ppois(a-1) is correct", {
  lambda <- 5; a <- 2; b <- 6
  app_result  <- ppois(b, lambda) - ppois(a - 1, lambda)
  manual_result <- sum(dpois(a:b, lambda))
  expect_equal(app_result, manual_result)
})
