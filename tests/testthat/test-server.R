library(testthat)
library(shiny)
library(ggplot2)

# testServer() is given the project root directory so that shiny loads the app
# (global.R, ui.R, server.R) with the correct working directory, which is needed
# for relative paths like www/css/styles.css used in ui.R.
app_dir <- normalizePath(file.path("..", ".."))

# Smoke tests using testServer() to verify that r_plot produces correct ggplot
# objects through the full server pipeline for representative distributions and
# tail types, in both PDF/PMF and CDF modes.

# --- Normal distribution (continuous, PDF mode) ---

test_that("Normal PDF lower tail: r_plot returns ggplot with correct title and y-axis", {
  testServer(app_dir, {
    session$setInputs(
      distribution = "Normal", plot_type = "pdf",
      mean_normal = 0, variance_sd = "sd_true", sd_normal = 1,
      lower_tail_normal = "lower.tail",
      x1_normal = 1, x2_normal = 1, a_normal = -1, b_normal = 1
    )
    p <- r_plot()
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Normal distribution: N(0, 1)")
    expect_equal(p$labels$y, "Density")
  })
})

# --- Normal distribution (continuous, CDF mode) ---

test_that("Normal CDF lower tail: r_plot returns ggplot with correct title, y-axis, and annotation value", {
  testServer(app_dir, {
    session$setInputs(
      distribution = "Normal", plot_type = "cdf",
      mean_normal = 0, variance_sd = "sd_true", sd_normal = 1,
      lower_tail_normal = "lower.tail",
      x1_normal = 0, x2_normal = 0, a_normal = -1, b_normal = 1
    )
    p <- r_plot()
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Normal distribution: N(0, 1)")
    expect_equal(p$labels$y, "Cumulative distribution function")
    # Annotation point y-value should equal pnorm(0) = 0.5
    expect_equal(p$layers[[4]]$data$y, pnorm(0, mean = 0, sd = 1))
  })
})

# --- Binomial distribution (discrete, CDF mode) ---

test_that("Binomial CDF lower tail: r_plot returns ggplot with correct title and y-axis", {
  testServer(app_dir, {
    session$setInputs(
      distribution = "Binomial", plot_type = "cdf",
      n_binomial = 10, p_binomial = 0.5,
      lower_tail_binomial = "lower.tail",
      x1_binomial = 4, x2_binomial = 4, a_binomial = 3, b_binomial = 7
    )
    p <- r_plot()
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Binomial distribution: Bin(10, 0.5)")
    expect_equal(p$labels$y, "Cumulative distribution function")
  })
})

# --- Poisson distribution (discrete, CDF mode) ---

test_that("Poisson CDF lower tail: r_plot returns ggplot with correct title", {
  testServer(app_dir, {
    session$setInputs(
      distribution = "Poisson", plot_type = "cdf",
      lambda_poisson = 3,
      lower_tail_poisson = "lower.tail",
      x1_poisson = 4, x2_poisson = 4, a_poisson = 2, b_poisson = 5
    )
    p <- r_plot()
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Poisson distribution: Pois(3)")
  })
})
