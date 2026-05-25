library(shiny)
library(shinycssloaders)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(mixdist)

options(shiny.mathjax.config = "config=TeX-AMS-MML_SVG")

cdf_continuous <- function(pfun, x_range, args, tail_type, x1, x2, a, b, title) {
  cdf_fn <- function(x) do.call(pfun, c(list(q = x), args))
  xl <- min(x_range)
  p <- ggplot(data.frame(x = x_range), aes(x = x)) +
    stat_function(fun = cdf_fn, n = 1000) +
    theme_minimal() +
    ggtitle(title) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Cumulative distribution function") +
    xlab("x") +
    ylim(0, 1)
  if (tail_type == "lower.tail") {
    fy <- cdf_fn(x1)
    p <- p +
      annotate("segment", x = x1, xend = x1, y = 0, yend = fy, linetype = "dashed", color = "gray40") +
      annotate("segment", x = xl, xend = x1, y = fy, yend = fy, linetype = "dashed", color = "gray40") +
      annotate("point", x = x1, y = fy, size = 3, color = "#4682B4")
  } else if (tail_type == "upper.tail") {
    fy <- cdf_fn(x2)
    p <- p +
      annotate("segment", x = x2, xend = x2, y = 0, yend = fy, linetype = "dashed", color = "gray40") +
      annotate("segment", x = xl, xend = x2, y = fy, yend = fy, linetype = "dashed", color = "gray40") +
      annotate("point", x = x2, y = fy, size = 3, color = "#4682B4")
  } else {
    fa <- cdf_fn(a)
    fb <- cdf_fn(b)
    p <- p +
      annotate("segment", x = a, xend = a, y = 0, yend = fa, linetype = "dashed", color = "gray40") +
      annotate("segment", x = b, xend = b, y = 0, yend = fb, linetype = "dashed", color = "gray40") +
      annotate("segment", x = xl, xend = a, y = fa, yend = fa, linetype = "dashed", color = "gray40") +
      annotate("segment", x = xl, xend = b, y = fb, yend = fb, linetype = "dashed", color = "gray40") +
      annotate("point", x = a, y = fa, size = 3, color = "#4682B4") +
      annotate("point", x = b, y = fb, size = 3, color = "#4682B4")
  }
  p
}

cdf_discrete <- function(x_vals, prob_vals, tail_type, x1, x2, a, b, title) {
  cdf_vals <- cumsum(prob_vals)
  xl <- x_vals[1] - 1
  x_step <- c(xl, x_vals)
  cdf_step <- c(0, cdf_vals)
  df_step <- data.frame(x = x_step, cdf = cdf_step)
  df_pts <- data.frame(x = x_vals, cdf = cdf_vals)
  p <- ggplot(df_step, aes(x = x, y = cdf)) +
    geom_step(direction = "hv") +
    geom_point(data = df_pts, size = 2) +
    theme_minimal() +
    ggtitle(title) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Cumulative distribution function") +
    xlab("x") +
    ylim(0, 1)
  if (tail_type == "lower.tail" && x1 %in% x_vals) {
    fy <- cdf_vals[match(x1, x_vals)]
    p <- p +
      annotate("segment", x = x1, xend = x1, y = 0, yend = fy, linetype = "dashed", color = "gray40") +
      annotate("segment", x = xl, xend = x1, y = fy, yend = fy, linetype = "dashed", color = "gray40") +
      annotate("point", x = x1, y = fy, size = 4, color = "#4682B4")
  } else if (tail_type == "upper.tail" && x2 %in% x_vals) {
    fy <- cdf_vals[match(x2, x_vals)]
    p <- p +
      annotate("segment", x = x2, xend = x2, y = 0, yend = fy, linetype = "dashed", color = "gray40") +
      annotate("segment", x = xl, xend = x2, y = fy, yend = fy, linetype = "dashed", color = "gray40") +
      annotate("point", x = x2, y = fy, size = 4, color = "#4682B4")
  } else if (tail_type == "interval") {
    if (a %in% x_vals) {
      fa <- cdf_vals[match(a, x_vals)]
      p <- p +
        annotate("segment", x = a, xend = a, y = 0, yend = fa, linetype = "dashed", color = "gray40") +
        annotate("segment", x = xl, xend = a, y = fa, yend = fa, linetype = "dashed", color = "gray40") +
        annotate("point", x = a, y = fa, size = 4, color = "#4682B4")
    }
    if (b %in% x_vals) {
      fb <- cdf_vals[match(b, x_vals)]
      p <- p +
        annotate("segment", x = b, xend = b, y = 0, yend = fb, linetype = "dashed", color = "gray40") +
        annotate("segment", x = xl, xend = b, y = fb, yend = fb, linetype = "dashed", color = "gray40") +
        annotate("point", x = b, y = fb, size = 4, color = "#4682B4")
    }
  }
  p
}
