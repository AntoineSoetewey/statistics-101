library(testthat)
library(ggplot2)

source(file.path("..", "..", "global.R"))

test_that("cdf_continuous returns a ggplot for lower tail", {
  p <- cdf_continuous(
    pfun = pnorm,
    x_range = c(-4, 4),
    args = list(mean = 0, sd = 1),
    tail_type = "lower.tail",
    x1 = 1, x2 = NULL, a = NULL, b = NULL,
    title = "Normal"
  )
  expect_s3_class(p, "ggplot")
})

test_that("cdf_continuous returns a ggplot for upper tail", {
  p <- cdf_continuous(
    pfun = pnorm,
    x_range = c(-4, 4),
    args = list(mean = 0, sd = 1),
    tail_type = "upper.tail",
    x1 = NULL, x2 = 1, a = NULL, b = NULL,
    title = "Normal"
  )
  expect_s3_class(p, "ggplot")
})

test_that("cdf_continuous returns a ggplot for interval", {
  p <- cdf_continuous(
    pfun = pnorm,
    x_range = c(-4, 4),
    args = list(mean = 0, sd = 1),
    tail_type = "interval",
    x1 = NULL, x2 = NULL, a = -1, b = 1,
    title = "Normal"
  )
  expect_s3_class(p, "ggplot")
})

test_that("cdf_continuous works for exponential distribution", {
  p <- cdf_continuous(
    pfun = pexp,
    x_range = c(0, 10),
    args = list(rate = 1),
    tail_type = "lower.tail",
    x1 = 2, x2 = NULL, a = NULL, b = NULL,
    title = "Exponential"
  )
  expect_s3_class(p, "ggplot")
})

test_that("cdf_continuous works for chi-square distribution", {
  p <- cdf_continuous(
    pfun = pchisq,
    x_range = c(0, 20),
    args = list(df = 5),
    tail_type = "upper.tail",
    x1 = NULL, x2 = 10, a = NULL, b = NULL,
    title = "Chi-square"
  )
  expect_s3_class(p, "ggplot")
})

test_that("cdf_discrete returns a ggplot for lower tail", {
  x_vals <- 0:10
  prob_vals <- dbinom(x_vals, size = 10, prob = 0.5)
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = prob_vals,
    tail_type = "lower.tail",
    x1 = 4, x2 = NULL, a = NULL, b = NULL,
    title = "Binomial"
  )
  expect_s3_class(p, "ggplot")
})

test_that("cdf_discrete returns a ggplot for upper tail", {
  x_vals <- 0:10
  prob_vals <- dbinom(x_vals, size = 10, prob = 0.5)
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = prob_vals,
    tail_type = "upper.tail",
    x1 = NULL, x2 = 6, a = NULL, b = NULL,
    title = "Binomial"
  )
  expect_s3_class(p, "ggplot")
})

test_that("cdf_discrete returns a ggplot for interval", {
  x_vals <- 0:10
  prob_vals <- dbinom(x_vals, size = 10, prob = 0.5)
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = prob_vals,
    tail_type = "interval",
    x1 = NULL, x2 = NULL, a = 3, b = 7,
    title = "Binomial"
  )
  expect_s3_class(p, "ggplot")
})

test_that("cdf_discrete handles x1 not in x_vals gracefully", {
  x_vals <- 0:5
  prob_vals <- dpois(x_vals, lambda = 2)
  prob_vals <- prob_vals / sum(prob_vals)
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = prob_vals,
    tail_type = "lower.tail",
    x1 = 10, x2 = NULL, a = NULL, b = NULL,
    title = "Poisson"
  )
  expect_s3_class(p, "ggplot")
})

test_that("cdf_discrete handles partial interval (only a in x_vals)", {
  x_vals <- 0:5
  prob_vals <- dpois(x_vals, lambda = 2)
  prob_vals <- prob_vals / sum(prob_vals)
  p <- cdf_discrete(
    x_vals = x_vals, prob_vals = prob_vals,
    tail_type = "interval",
    x1 = NULL, x2 = NULL, a = 2, b = 99,
    title = "Poisson"
  )
  expect_s3_class(p, "ggplot")
})
