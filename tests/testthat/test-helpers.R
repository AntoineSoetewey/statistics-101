library(testthat)
library(ggplot2)

source(file.path("..", "..", "global.R"))

# --- cdf_continuous: plot metadata ---

test_that("cdf_continuous: returns ggplot with correct title and axis labels", {
  p <- cdf_continuous(
    pfun = pnorm, x_range = c(-4, 4), args = list(mean = 0, sd = 1),
    tail_type = "lower.tail", x1 = 0, x2 = NULL, a = NULL, b = NULL,
    title = "My title"
  )
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "My title")
  expect_equal(p$labels$y, "Cumulative distribution function")
  expect_equal(p$labels$x, "x")
})

# --- cdf_continuous: layer counts per tail type ---
# Base: 1 layer (stat_function)
# Lower / upper tail: +2 segments +1 point = 4 total
# Interval: +4 segments +2 points = 7 total

test_that("cdf_continuous lower tail: 4 layers (curve + 2 segments + 1 point)", {
  p <- cdf_continuous(
    pfun = pnorm, x_range = c(-4, 4), args = list(mean = 0, sd = 1),
    tail_type = "lower.tail", x1 = 1, x2 = NULL, a = NULL, b = NULL,
    title = "Normal"
  )
  expect_equal(length(p$layers), 4)
})

test_that("cdf_continuous upper tail: 4 layers (curve + 2 segments + 1 point)", {
  p <- cdf_continuous(
    pfun = pexp, x_range = c(0, 10), args = list(rate = 1),
    tail_type = "upper.tail", x1 = NULL, x2 = 2, a = NULL, b = NULL,
    title = "Exponential"
  )
  expect_equal(length(p$layers), 4)
})

test_that("cdf_continuous interval: 7 layers (curve + 4 segments + 2 points)", {
  p <- cdf_continuous(
    pfun = pnorm, x_range = c(-4, 4), args = list(mean = 0, sd = 1),
    tail_type = "interval", x1 = NULL, x2 = NULL, a = -1, b = 1,
    title = "Normal"
  )
  expect_equal(length(p$layers), 7)
})

# --- cdf_continuous: annotation point y-values match the CDF ---

test_that("cdf_continuous lower tail: annotation point y equals CDF at x1", {
  p <- cdf_continuous(
    pfun = pnorm, x_range = c(-4, 4), args = list(mean = 0, sd = 1),
    tail_type = "lower.tail", x1 = 0, x2 = NULL, a = NULL, b = NULL,
    title = "Normal"
  )
  point_layer <- p$layers[[4]]
  expect_equal(point_layer$data$y, pnorm(0, mean = 0, sd = 1))
})

test_that("cdf_continuous upper tail: annotation point y equals CDF at x2", {
  p <- cdf_continuous(
    pfun = pchisq, x_range = c(0, 20), args = list(df = 5),
    tail_type = "upper.tail", x1 = NULL, x2 = 5, a = NULL, b = NULL,
    title = "Chi-square"
  )
  point_layer <- p$layers[[4]]
  expect_equal(point_layer$data$y, pchisq(5, df = 5))
})

test_that("cdf_continuous interval: annotation points y equal CDF at a and b", {
  p <- cdf_continuous(
    pfun = pnorm, x_range = c(-4, 4), args = list(mean = 0, sd = 1),
    tail_type = "interval", x1 = NULL, x2 = NULL, a = -1, b = 2,
    title = "Normal"
  )
  expect_equal(p$layers[[6]]$data$y, pnorm(-1))
  expect_equal(p$layers[[7]]$data$y, pnorm(2))
})

# --- cdf_discrete: plot metadata ---

test_that("cdf_discrete: returns ggplot with correct title and axis labels", {
  x_vals <- 0:10
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = dbinom(x_vals, 10, 0.5),
    tail_type = "lower.tail", x1 = 4, x2 = NULL, a = NULL, b = NULL,
    title = "Binomial"
  )
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Binomial")
  expect_equal(p$labels$y, "Cumulative distribution function")
  expect_equal(p$labels$x, "x")
})

# --- cdf_discrete: layer counts per tail type ---
# Base: 2 layers (geom_step + geom_point for all mass points)
# Lower/upper tail (x in x_vals): +2 segments +1 point = 5 total
# Lower/upper tail (x NOT in x_vals): no extra layers = 2 total
# Interval (both endpoints in x_vals): +4 segments +2 points = 8 total
# Interval (only one endpoint in x_vals): +2 segments +1 point = 5 total

test_that("cdf_discrete lower tail (x1 in x_vals): 5 layers", {
  x_vals <- 0:10
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = dbinom(x_vals, 10, 0.5),
    tail_type = "lower.tail", x1 = 4, x2 = NULL, a = NULL, b = NULL,
    title = "Binomial"
  )
  expect_equal(length(p$layers), 5)
})

test_that("cdf_discrete lower tail (x1 NOT in x_vals): 2 layers, no annotation", {
  x_vals <- 0:5
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = dpois(x_vals, 2) / sum(dpois(x_vals, 2)),
    tail_type = "lower.tail", x1 = 99, x2 = NULL, a = NULL, b = NULL,
    title = "Poisson"
  )
  expect_equal(length(p$layers), 2)
})

test_that("cdf_discrete upper tail (x2 in x_vals): 5 layers", {
  x_vals <- 0:10
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = dbinom(x_vals, 10, 0.5),
    tail_type = "upper.tail", x1 = NULL, x2 = 7, a = NULL, b = NULL,
    title = "Binomial"
  )
  expect_equal(length(p$layers), 5)
})

test_that("cdf_discrete interval (both a and b in x_vals): 8 layers", {
  x_vals <- 0:10
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = dbinom(x_vals, 10, 0.5),
    tail_type = "interval", x1 = NULL, x2 = NULL, a = 3, b = 7,
    title = "Binomial"
  )
  expect_equal(length(p$layers), 8)
})

test_that("cdf_discrete interval (only a in x_vals): 5 layers", {
  x_vals <- 0:5
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = dpois(x_vals, 2) / sum(dpois(x_vals, 2)),
    tail_type = "interval", x1 = NULL, x2 = NULL, a = 2, b = 99,
    title = "Poisson"
  )
  expect_equal(length(p$layers), 5)
})

# --- cdf_discrete: annotation point y-values match the cumulative probabilities ---

test_that("cdf_discrete lower tail: annotation point y equals cumulative prob at x1", {
  x_vals <- 0:10
  prob_vals <- dbinom(x_vals, 10, 0.5)
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = prob_vals,
    tail_type = "lower.tail", x1 = 5, x2 = NULL, a = NULL, b = NULL,
    title = "Binomial"
  )
  expected_cdf <- cumsum(prob_vals)[match(5, x_vals)]
  expect_equal(p$layers[[5]]$data$y, expected_cdf)
})

test_that("cdf_discrete interval: annotation points y match cumulative probs at a and b", {
  x_vals <- 0:10
  prob_vals <- dbinom(x_vals, 10, 0.5)
  cdf_vals <- cumsum(prob_vals)
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = prob_vals,
    tail_type = "interval", x1 = NULL, x2 = NULL, a = 3, b = 7,
    title = "Binomial"
  )
  expect_equal(p$layers[[5]]$data$y, cdf_vals[match(3, x_vals)])
  expect_equal(p$layers[[8]]$data$y, cdf_vals[match(7, x_vals)])
})

# --- cdf_discrete: step data construction ---

test_that("cdf_discrete: step data starts one unit left of support with CDF = 0", {
  x_vals <- 3:8
  prob_vals <- rep(1 / 6, 6)
  # xl should be x_vals[1] - 1 = 2; first step point is (2, 0)
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = prob_vals,
    tail_type = "lower.tail", x1 = 5, x2 = NULL, a = NULL, b = NULL,
    title = "Uniform discrete"
  )
  step_data <- p$data  # global plot data (geom_step inherits from it)
  expect_equal(step_data$x[1], 2)
  expect_equal(step_data$cdf[1], 0)
})

test_that("cdf_discrete: final step data point has CDF = 1", {
  x_vals <- 0:4
  prob_vals <- rep(0.2, 5)
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = prob_vals,
    tail_type = "lower.tail", x1 = 2, x2 = NULL, a = NULL, b = NULL,
    title = "Uniform discrete"
  )
  step_data <- p$data  # global plot data (geom_step inherits from it)
  expect_equal(round(tail(step_data$cdf, 1), 10), 1)
})
