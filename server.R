server <- function(input, output) {
  output$results_distribution <- renderUI({
    if (input$distribution == "Beta") {
      withMathJax(
        paste0("\\(X \\sim Beta(\\alpha = \\)", " ", input$alpha_beta, ", ", "\\(\\beta = \\)", " ", input$beta_beta, "\\()\\)", " and ", case_when(
          input$lower_tail_beta == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_beta, "\\()\\)", " ", "\\( = \\)", " ", round(pbeta(input$x1_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE), 4)),
          input$lower_tail_beta == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_beta, "\\()\\)", " ", "\\( = \\)", " ", round(pbeta(input$x2_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), 4)),
          input$lower_tail_beta == "interval" ~ paste0("\\(P(\\)", input$a_beta, " ", "\\(\\leq X\\leq \\)", " ", input$b_beta, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_beta > input$b_beta, "a must be less than or equal to b", round(pbeta(input$b_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE) - pbeta(input$a_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Binomial") {
      withMathJax(
        paste0("\\(X \\sim Bin(n = \\)", " ", input$n_binomial, ", ", "\\(p = \\)", " ", input$p_binomial, "\\()\\)", " and ", case_when(
          input$lower_tail_binomial == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_binomial, "\\()\\)", " ", "\\( = \\)", " ", round(pbinom(input$x1_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), 4)),
          input$lower_tail_binomial == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_binomial, "\\()\\)", " ", "\\( = \\)", " ", round(pbinom(input$x2_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE), 4)),
          input$lower_tail_binomial == "interval" ~ paste0("\\(P(\\)", input$a_binomial, " ", "\\(\\leq X\\leq \\)", " ", input$b_binomial, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_binomial > input$b_binomial, "a must be less than or equal to b", round(pbinom(input$b_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE) - pbinom(input$a_binomial - 1, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Cauchy") {
      withMathJax(
        paste0("\\(X \\sim Cauchy(x_0 = \\)", " ", input$location_cauchy, ", ", "\\(\\gamma = \\)", " ", input$scale_cauchy, "\\()\\)", " and ", case_when(
          input$lower_tail_cauchy == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_cauchy, "\\()\\)", " ", "\\( = \\)", " ", round(pcauchy(input$x1_cauchy, location = input$location_cauchy, scale = input$scale_cauchy, lower.tail = TRUE), 4)),
          input$lower_tail_cauchy == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_cauchy, "\\()\\)", " ", "\\( = \\)", " ", round(pcauchy(input$x2_cauchy, location = input$location_cauchy, scale = input$scale_cauchy, lower.tail = FALSE), 4)),
          input$lower_tail_cauchy == "interval" ~ paste0("\\(P(\\)", input$a_cauchy, " ", "\\(\\leq X\\leq \\)", " ", input$b_cauchy, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_cauchy > input$b_cauchy, "a must be less than or equal to b", round(pcauchy(input$b_cauchy, location = input$location_cauchy, scale = input$scale_cauchy, lower.tail = TRUE) - pcauchy(input$a_cauchy, location = input$location_cauchy, scale = input$scale_cauchy, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Chi-square") {
      withMathJax(
        paste0("\\(X \\sim \\chi^2(df = \\)", " ", input$df_chisquare, "\\()\\)", " and ", case_when(
          input$lower_tail_chisquare == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_chisquare, "\\()\\)", " ", "\\( = \\)", " ", round(pchisq(input$x1_chisquare, df = input$df_chisquare, lower.tail = TRUE), 4)),
          input$lower_tail_chisquare == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_chisquare, "\\()\\)", " ", "\\( = \\)", " ", round(pchisq(input$x2_chisquare, df = input$df_chisquare, lower.tail = FALSE), 4)),
          input$lower_tail_chisquare == "interval" ~ paste0("\\(P(\\)", input$a_chisquare, " ", "\\(\\leq X\\leq \\)", " ", input$b_chisquare, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_chisquare > input$b_chisquare, "a must be less than or equal to b", round(pchisq(input$b_chisquare, df = input$df_chisquare, lower.tail = TRUE) - pchisq(input$a_chisquare, df = input$df_chisquare, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Exponential") {
      withMathJax(
        paste0("\\(X \\sim Exp(\\lambda = \\)", " ", input$rate_exponential, "\\()\\)", " and ", case_when(
          input$lower_tail_exponential == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_exponential, "\\()\\)", " ", "\\( = \\)", " ", round(pexp(input$x1_exponential, rate = input$rate_exponential, lower.tail = TRUE), 4)),
          input$lower_tail_exponential == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_exponential, "\\()\\)", " ", "\\( = \\)", " ", round(pexp(input$x2_exponential, rate = input$rate_exponential, lower.tail = FALSE), 4)),
          input$lower_tail_exponential == "interval" ~ paste0("\\(P(\\)", input$a_exponential, " ", "\\(\\leq X\\leq \\)", " ", input$b_exponential, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_exponential > input$b_exponential, "a must be less than or equal to b", round(pexp(input$b_exponential, rate = input$rate_exponential, lower.tail = TRUE) - pexp(input$a_exponential, rate = input$rate_exponential, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Fisher") {
      withMathJax(
        paste0("\\(X \\sim F(df_1 = \\)", " ", input$df1_fisher, ", ", "\\(df_2\\)", " = ", input$df2_fisher, "\\()\\)", " and ", case_when(
          input$lower_tail_fisher == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_fisher, "\\()\\)", " ", "\\( = \\)", " ", round(pf(input$x1_fisher, df1 = input$df1_fisher, df2 = input$df2_fisher, lower.tail = TRUE), 4)),
          input$lower_tail_fisher == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_fisher, "\\()\\)", " ", "\\( = \\)", " ", round(pf(input$x2_fisher, df1 = input$df1_fisher, df2 = input$df2_fisher, lower.tail = FALSE), 4)),
          input$lower_tail_fisher == "interval" ~ paste0("\\(P(\\)", input$a_fisher, " ", "\\(\\leq X\\leq \\)", " ", input$b_fisher, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_fisher > input$b_fisher, "a must be less than or equal to b", round(pf(input$b_fisher, df1 = input$df1_fisher, df = input$df2_fisher, lower.tail = TRUE) - pf(input$a_fisher, df1 = input$df1_fisher, df = input$df2_fisher, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Gamma") {
      withMathJax(
        paste0("\\(X \\sim Gamma(\\alpha = \\)", " ", input$alpha_gamma, ", ", "\\(\\beta = \\)", " ", input$beta_gamma, "\\()\\)", " and ", case_when(
          input$lower_tail_gamma == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_gamma, "\\()\\)", " ", "\\( = \\)", " ", round(pgamma(input$x1_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE), 4)),
          input$lower_tail_gamma == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_gamma, "\\()\\)", " ", "\\( = \\)", " ", round(pgamma(input$x2_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), 4)),
          input$lower_tail_gamma == "interval" ~ paste0("\\(P(\\)", input$a_gamma, " ", "\\(\\leq X\\leq \\)", " ", input$b_gamma, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_gamma > input$b_gamma, "a must be less than or equal to b", round(pgamma(input$b_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE) - pgamma(input$a_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Geometric (I)") {
      withMathJax(
        paste0("\\(X \\sim Geom(p = \\)", " ", input$p_geometric, "\\()\\)", " and ", case_when(
          input$lower_tail_geometric == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_geometric, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x1_geometric, prob = input$p_geometric, lower.tail = TRUE), 4)),
          input$lower_tail_geometric == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_geometric, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x2_geometric, prob = input$p_geometric, lower.tail = FALSE), 4)),
          input$lower_tail_geometric == "interval" ~ paste0("\\(P(\\)", input$a_geometric, " ", "\\(\\leq X\\leq \\)", " ", input$b_geometric, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_geometric > input$b_geometric, "a must be less than or equal to b", round(pgeom(input$b_geometric, prob = input$p_geometric, lower.tail = TRUE) - pgeom(input$a_geometric - 1, prob = input$p_geometric, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Geometric (II)") {
      withMathJax(
        paste0("\\(X \\sim Geom(p = \\)", " ", input$p_geometric2, "\\()\\)", " and ", case_when(
          input$lower_tail_geometric2 == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_geometric2, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x1_geometric2 - 1, prob = input$p_geometric2, lower.tail = TRUE), 4)),
          input$lower_tail_geometric2 == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_geometric2, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x2_geometric2 - 1, prob = input$p_geometric2, lower.tail = FALSE), 4)),
          input$lower_tail_geometric2 == "interval" ~ paste0("\\(P(\\)", input$a_geometric2, " ", "\\(\\leq X\\leq \\)", " ", input$b_geometric2, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_geometric2 > input$b_geometric2, "a must be less than or equal to b", round(pgeom(input$b_geometric2 - 1, prob = input$p_geometric2, lower.tail = TRUE) - pgeom(input$a_geometric2 - 2, prob = input$p_geometric2, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Hypergeometric") {
      withMathJax(
        paste0("\\(X \\sim HG(n = \\)", " ", input$n_hypergeometric, ", ", "\\(N = \\)", " ", input$N_hypergeometric, ", ", "\\(M = \\)", " ", input$M_hypergeometric, "\\()\\)", " and ", case_when(
          input$lower_tail_hypergeometric == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", round(phyper(input$x1_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), 4)),
          input$lower_tail_hypergeometric == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", round(phyper(input$x2_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE), 4)),
          input$lower_tail_hypergeometric == "interval" ~ paste0("\\(P(\\)", input$a_hypergeometric, " ", "\\(\\leq X\\leq \\)", " ", input$b_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_hypergeometric > input$b_hypergeometric, "a must be less than or equal to b", round(phyper(input$b_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE) - phyper(input$a_hypergeometric - 1, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Logistic") {
      withMathJax(
        paste0("\\(X \\sim Logi(\\mu = \\)", " ", input$location_logistic, ", ", "\\(s = \\)", " ", input$scale_logistic, "\\()\\)", " and ", case_when(
          input$lower_tail_logistic == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_logistic, "\\()\\)", " ", "\\( = \\)", " ", round(plogis(input$x1_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE), 4)),
          input$lower_tail_logistic == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_logistic, "\\()\\)", " ", "\\( = \\)", " ", round(plogis(input$x2_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), 4)),
          input$lower_tail_logistic == "interval" ~ paste0("\\(P(\\)", input$a_logistic, " ", "\\(\\leq X\\leq \\)", " ", input$b_logistic, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_logistic > input$b_logistic, "a must be less than or equal to b", round(plogis(input$b_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE) - plogis(input$a_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Log-Normal") {
      withMathJax(
        paste0("\\(X \\sim Lognormal(\\mu = \\)", " ", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", paste0("\\(\\sigma^2 = \\)", " ", input$variance_lognormal), paste0("\\(\\sigma^2 = \\)", " ", input$sd_lognormal^2)), "\\()\\)", " and ", case_when(
          input$lower_tail_lognormal == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_lognormal, "\\()\\)", " ", "\\( = \\)", " ", round(plnorm(input$x1_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE), 4)),
          input$lower_tail_lognormal == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_lognormal, "\\()\\)", " ", "\\( = \\)", " ", "\\( = \\)", " ", round(plnorm(input$x2_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = FALSE), 4)),
          input$lower_tail_lognormal == "interval" ~ paste0("\\(P(\\)", input$a_lognormal, " ", "\\(\\leq X\\leq \\)", " ", input$b_lognormal, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_lognormal > input$b_lognormal, "a must be less than or equal to b", round(plnorm(input$b_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE) - plnorm(input$a_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Negative Binomial (I)") {
      withMathJax(
        paste0("\\(X \\sim NG(r = \\)", " ", input$r_negativebinomial, ", ", "\\(p = \\)", " ", input$p_negativebinomial, "\\()\\)", " and ", case_when(
          input$lower_tail_negativebinomial == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$x1_negativebinomial, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), 4)),
          input$lower_tail_negativebinomial == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$x2_negativebinomial, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE), 4)),
          input$lower_tail_negativebinomial == "interval" ~ paste0("\\(P(\\)", input$a_negativebinomial, " ", "\\(\\leq X\\leq \\)", " ", input$b_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_negativebinomial > input$b_negativebinomial, "a must be less than or equal to b", round(pnbinom(input$b_negativebinomial, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE) - pnbinom(input$a_negativebinomial - 1, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Negative Binomial (II)") {
      withMathJax(
        paste0("\\(X \\sim NG(r = \\)", " ", input$r_negativebinomial2, ", ", "\\(p = \\)", " ", input$p_negativebinomial2, "\\()\\)", " and ", case_when(
          input$lower_tail_negativebinomial2 == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_negativebinomial2, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$x1_negativebinomial2 - input$r_negativebinomial2, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE), 4)),
          input$lower_tail_negativebinomial2 == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_negativebinomial2, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$x2_negativebinomial2 - input$r_negativebinomial2, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = FALSE), 4)),
          input$lower_tail_negativebinomial2 == "interval" ~ paste0("\\(P(\\)", input$a_negativebinomial2, " ", "\\(\\leq X\\leq \\)", " ", input$b_negativebinomial2, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_negativebinomial2 > input$b_negativebinomial2, "a must be less than or equal to b", round(pnbinom(input$b_negativebinomial2 - input$r_negativebinomial2, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE) - pnbinom(input$a_negativebinomial2 - 1 - input$r_negativebinomial2, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Normal") {
      withMathJax(
        paste0("\\(X \\sim N(\\mu = \\)", " ", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", paste0("\\(\\sigma^2 = \\)", " ", input$variance_normal), paste0("\\(\\sigma^2 = \\)", " ", input$sd_normal^2)), "\\()\\)", " and ", case_when(
          input$lower_tail_normal == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_normal, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z \\leq \\)", " ", "\\((\\)", input$x1_normal, " ", "\\(-\\)", " ", input$mean_normal, "\\()\\)", " ", "\\(/\\)", " ", ifelse(input$variance_sd == "variance_true", round(sqrt(input$variance_normal), 2), input$sd_normal), "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z \\leq \\)", " ", round((input$x1_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(input$x1_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE), 4)),
          input$lower_tail_normal == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_normal, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z > \\)", " ", "\\((\\)", input$x2_normal, " ", "\\(-\\)", " ", input$mean_normal, "\\()\\)", " ", "\\(/\\)", " ", ifelse(input$variance_sd == "variance_true", round(sqrt(input$variance_normal), 2), input$sd_normal), "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z > \\)", " ", round((input$x2_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(input$x2_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), 4)),
          input$lower_tail_normal == "interval" ~ paste0("\\(P(\\)", input$a_normal, " ", "\\(\\leq X\\leq \\)", " ", input$b_normal, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(\\)", "\\((\\)", input$a_normal, " ", "\\(-\\)", " ", input$mean_normal, "\\()\\)", " ", "\\(/\\)", " ", ifelse(input$variance_sd == "variance_true", round(sqrt(input$variance_normal), 2), input$sd_normal), "\\()\\)", " ", "\\(\\leq Z\\leq \\)", " ", "\\((\\)", input$b_normal, " ", "\\(-\\)", " ", input$mean_normal, "\\()\\)", " ", "\\(/\\)", " ", ifelse(input$variance_sd == "variance_true", round(sqrt(input$variance_normal), 2), input$sd_normal), "\\()\\)", " ", "\\( = \\)", " ","\\(P(\\)", round((input$a_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), " ", "\\(\\leq Z\\leq \\)", " ", round((input$b_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_normal > input$b_normal, "a must be less than or equal to b", round(pnorm(input$b_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE) - pnorm(input$a_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Poisson") {
      withMathJax(
        paste0("\\(X \\sim Pois(\\lambda = \\)", " ", input$lambda_poisson, "\\()\\)", " and ", case_when(
          input$lower_tail_poisson == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_poisson, "\\()\\)", " ", "\\( = \\)", " ", round(ppois(input$x1_poisson, lambda = input$lambda_poisson, lower.tail = TRUE), 4)),
          input$lower_tail_poisson == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_poisson, "\\()\\)", " ", "\\( = \\)", " ", round(ppois(input$x2_poisson, lambda = input$lambda_poisson, lower.tail = FALSE), 4)),
          input$lower_tail_poisson == "interval" ~ paste0("\\(P(\\)", input$a_poisson, " ", "\\(\\leq X\\leq \\)", " ", input$b_poisson, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_poisson > input$b_poisson, "a must be less than or equal to b", round(ppois(input$b_poisson, lambda = input$lambda_poisson, lower.tail = TRUE) - ppois(input$a_poisson - 1, lambda = input$lambda_poisson, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Student") {
      withMathJax(
        paste0("\\(X \\sim St(df = \\)", " ", input$df_student, "\\()\\)", " and ", case_when(
          input$lower_tail_student == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_student, "\\()\\)", " ", "\\( = \\)", " ", round(pt(input$x1_student, df = input$df_student, lower.tail = TRUE), 4)),
          input$lower_tail_student == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_student, "\\()\\)", " ", "\\( = \\)", " ", round(pt(input$x2_student, df = input$df_student, lower.tail = FALSE), 4)),
          input$lower_tail_student == "interval" ~ paste0("\\(P(\\)", input$a_student, " ", "\\(\\leq X\\leq \\)", " ", input$b_student, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_student > input$b_student, "a must be less than or equal to b", round(pt(input$b_student, df = input$df_student, lower.tail = TRUE) - pt(input$a_student, df = input$df_student, lower.tail = TRUE), 4)))
        ))
      )
    } else if (input$distribution == "Weibull") {
      withMathJax(
        paste0("\\(X \\sim Weibull(\\alpha = \\)", " ", input$alpha_weibull, ", ", "\\(\\beta = \\)", " ", input$beta_weibull, "\\()\\)", " and ", case_when(
          input$lower_tail_weibull == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_weibull, "\\()\\)", " ", "\\( = \\)", " ", round(pweibull(input$x1_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE), 4)),
          input$lower_tail_weibull == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_weibull, "\\()\\)", " ", "\\( = \\)", " ", round(pweibull(input$x2_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), 4)),
          input$lower_tail_weibull == "interval" ~ paste0("\\(P(\\)", input$a_weibull, " ", "\\(\\leq X\\leq \\)", " ", input$b_weibull, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_weibull > input$b_weibull, "a must be less than or equal to b", round(pweibull(input$b_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE) - pweibull(input$a_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE), 4)))
        ))
      )
    } else {
      print("loading...")
    }
  })
  
  # reactive to contain plot:
  r_plot <- reactive({
    res <- if (input$distribution == "Beta" && input$lower_tail_beta == "lower.tail") {
      funcShaded <- function(x) {
        y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
        y[x > input$x1_beta] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Beta(", input$alpha_beta, ", ", input$beta_beta, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Beta' && input$lower_tail_beta == 'upper.tail') {
      funcShaded <- function(x) {
        y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
        y[x < input$x2_beta] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Beta(", input$alpha_beta, ", ", input$beta_beta, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Beta' && input$lower_tail_beta == 'interval') {
      funcShaded <- function(x) {
        y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
        y[x < input$a_beta | x > input$b_beta] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Beta(", input$alpha_beta, ", ", input$beta_beta, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Binomial' && input$lower_tail_binomial == 'lower.tail') {
      p <- data.frame(heads = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), prob = dbinom(x = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), size = input$n_binomial, prob = input$p_binomial)) %>%
        mutate(Heads = ifelse(heads <= input$x1_binomial, "2", "Other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: Bin(", input$n_binomial, ", ", input$p_binomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Binomial' && input$lower_tail_binomial == 'upper.tail') {
      p <- data.frame(heads = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), prob = dbinom(x = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), size = input$n_binomial, prob = input$p_binomial)) %>%
        mutate(Heads = ifelse(heads > input$x2_binomial, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: Bin(", input$n_binomial, ", ", input$p_binomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Binomial' && input$lower_tail_binomial == 'interval') {
      p <- data.frame(heads = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), prob = dbinom(x = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), size = input$n_binomial, prob = input$p_binomial)) %>%
        mutate(Heads = ifelse(heads >= input$a_binomial & heads <= input$b_binomial, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: Bin(", input$n_binomial, ", ", input$p_binomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Cauchy' && input$lower_tail_cauchy == 'lower.tail') {
      funcShaded <- function(x) {
        y <- dcauchy(x, location = input$location_cauchy, scale = input$scale_cauchy)
        y[x > input$x1_cauchy] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(input$location_cauchy - (6 * input$scale_cauchy), input$location_cauchy + (6 * input$scale_cauchy))), aes(x = x)) +
        stat_function(fun = dcauchy, args = list(location = input$location_cauchy, scale = input$scale_cauchy)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Cauchy(", input$location_cauchy, ", ", input$scale_cauchy, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Cauchy' && input$lower_tail_cauchy == 'upper.tail') {
      funcShaded <- function(x) {
        y <- dcauchy(x, location = input$location_cauchy, scale = input$scale_cauchy)
        y[x < input$x2_cauchy] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(input$location_cauchy - (6 * input$scale_cauchy), input$location_cauchy + (6 * input$scale_cauchy))), aes(x = x)) +
        stat_function(fun = dcauchy, args = list(location = input$location_cauchy, scale = input$scale_cauchy)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Cauchy(", input$location_cauchy, ", ", input$scale_cauchy, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Cauchy' && input$lower_tail_cauchy == 'interval') {
      funcShaded <- function(x) {
        y <- dcauchy(x, location = input$location_cauchy, scale = input$scale_cauchy)
        y[x < input$a_cauchy | x > input$b_cauchy] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(input$location_cauchy - (6 * input$scale_cauchy), input$location_cauchy + (6 * input$scale_cauchy))), aes(x = x)) +
        stat_function(fun = dcauchy, args = list(location = input$location_cauchy, scale = input$scale_cauchy)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Cauchy(", input$location_cauchy, ", ", input$scale_cauchy, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Chi-square' && input$lower_tail_chisquare == 'lower.tail') {
      funcShaded <- function(x) {
        y <- dchisq(x, df = input$df_chisquare)
        y[x > input$x1_chisquare] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qchisq(0.99999, df = input$df_chisquare, lower.tail = FALSE), qchisq(0.99999, df = input$df_chisquare, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Chi(", input$df_chisquare, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Chi-square' && input$lower_tail_chisquare == 'upper.tail') {
      funcShaded <- function(x) {
        y <- dchisq(x, df = input$df_chisquare)
        y[x < input$x2_chisquare] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qchisq(0.99999, df = input$df_chisquare, lower.tail = FALSE), qchisq(0.99999, df = input$df_chisquare, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Chi(", input$df_chisquare, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Chi-square' && input$lower_tail_chisquare == 'interval') {
      funcShaded <- function(x) {
        y <- dchisq(x, df = input$df_chisquare)
        y[x < input$a_chisquare | x > input$b_chisquare] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qchisq(0.99999, df = input$df_chisquare, lower.tail = FALSE), qchisq(0.99999, df = input$df_chisquare, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Chi(", input$df_chisquare, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Exponential' && input$lower_tail_exponential == 'lower.tail') {
      funcShaded <- function(x) {
        y <- dexp(x, rate = input$rate_exponential)
        y[x > input$x1_exponential] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qexp(0.99999, rate = input$rate_exponential, lower.tail = FALSE), qexp(0.99999, rate = input$rate_exponential, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Exp(", input$rate_exponential, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Exponential' && input$lower_tail_exponential == 'upper.tail') {
      funcShaded <- function(x) {
        y <- dexp(x, rate = input$rate_exponential)
        y[x < input$x2_exponential] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qexp(0.99999, rate = input$rate_exponential, lower.tail = FALSE), qexp(0.99999, rate = input$rate_exponential, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Exp(", input$rate_exponential, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Exponential' && input$lower_tail_exponential == 'interval') {
      funcShaded <- function(x) {
        y <- dexp(x, rate = input$rate_exponential)
        y[x < input$a_exponential | x > input$b_exponential] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qexp(0.99999, rate = input$rate_exponential, lower.tail = FALSE), qexp(0.99999, rate = input$rate_exponential, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Exp(", input$rate_exponential, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Fisher' && input$lower_tail_fisher == 'lower.tail') {
      funcShaded <- function(x) {
        y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
        y[x > input$x1_fisher] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
        stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: F(", input$df1_fisher, ", ", input$df2_fisher, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Fisher' && input$lower_tail_fisher == 'upper.tail') {
      funcShaded <- function(x) {
        y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
        y[x < input$x2_fisher] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
        stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: F(", input$df1_fisher, ", ", input$df2_fisher, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Fisher' && input$lower_tail_fisher == 'interval') {
      funcShaded <- function(x) {
        y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
        y[x < input$a_fisher | x > input$b_fisher] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
        stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: F(", input$df1_fisher, ", ", input$df2_fisher, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Gamma' && input$lower_tail_gamma == 'lower.tail') {
      funcShaded <- function(x) {
        y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
        y[x > input$x1_gamma] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Gamma(", input$alpha_gamma, ", ", input$beta_gamma, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Gamma' && input$lower_tail_gamma == 'upper.tail') {
      funcShaded <- function(x) {
        y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
        y[x < input$x2_gamma] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Gamma(", input$alpha_gamma, ", ", input$beta_gamma, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Gamma' && input$lower_tail_gamma == 'interval') {
      funcShaded <- function(x) {
        y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
        y[x < input$a_gamma | x > input$b_gamma] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Gamma(", input$alpha_gamma, ", ", input$beta_gamma, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Geometric (I)' && input$lower_tail_geometric == 'lower.tail') {
      p <- data.frame(heads = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = input$p_geometric)) %>%
        mutate(Heads = ifelse(heads <= input$x1_geometric, "2", "Other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: Geom(", input$p_geometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Geometric (I)' && input$lower_tail_geometric == 'upper.tail') {
      p <- data.frame(heads = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = input$p_geometric)) %>%
        mutate(Heads = ifelse(heads > input$x2_geometric, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: Geom(", input$p_geometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Geometric (I)' && input$lower_tail_geometric == 'interval') {
      p <- data.frame(heads = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = input$p_geometric)) %>%
        mutate(Heads = ifelse(heads >= input$a_geometric & heads <= input$b_geometric, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: Geom(", input$p_geometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Geometric (II)' && input$lower_tail_geometric2 == 'lower.tail') {
      p <- data.frame(heads = 1:(input$p_geometric2 + (5 * sqrt((1 - input$p_geometric2) / (input$p_geometric2^2))) + 1), prob = dgeom(x = 0:(input$p_geometric2 + (5 * sqrt((1 - input$p_geometric2) / (input$p_geometric2^2)))), prob = input$p_geometric2)) %>%
        mutate(Heads = ifelse(heads <= input$x1_geometric2, "2", "Other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: Geom(", input$p_geometric2, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Geometric (II)' && input$lower_tail_geometric2 == 'upper.tail') {
      p <- data.frame(heads = 1:(input$p_geometric2 + (5 * sqrt((1 - input$p_geometric2) / (input$p_geometric2^2))) + 1), prob = dgeom(x = 0:(input$p_geometric2 + (5 * sqrt((1 - input$p_geometric2) / (input$p_geometric2^2)))), prob = input$p_geometric2)) %>%
        mutate(Heads = ifelse(heads > input$x2_geometric2, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: Geom(", input$p_geometric2, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Geometric (II)' && input$lower_tail_geometric2 == 'interval') {
      p <- data.frame(heads = 1:(input$p_geometric2 + (5 * sqrt((1 - input$p_geometric2) / (input$p_geometric2^2))) + 1), prob = dgeom(x = 0:(input$p_geometric2 + (5 * sqrt((1 - input$p_geometric2) / (input$p_geometric2^2)))), prob = input$p_geometric2)) %>%
        mutate(Heads = ifelse(heads >= input$a_geometric2 & heads <= input$b_geometric2, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: Geom(", input$p_geometric2, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Hypergeometric' && input$lower_tail_hypergeometric == 'lower.tail') {
      p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
        mutate(Heads = ifelse(heads <= input$x1_hypergeometric, "2", "Other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: HG(", input$n_hypergeometric, ", ", input$N_hypergeometric, ", ", input$M_hypergeometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Hypergeometric' && input$lower_tail_hypergeometric == 'upper.tail') {
      p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
        mutate(Heads = ifelse(heads > input$x2_hypergeometric, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: HG(", input$n_hypergeometric, ", ", input$N_hypergeometric, ", ", input$M_hypergeometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Hypergeometric' && input$lower_tail_hypergeometric == 'interval') {
      p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
        mutate(Heads = ifelse(heads >= input$a_hypergeometric & heads <= input$b_hypergeometric, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: HG(", input$n_hypergeometric, ", ", input$N_hypergeometric, ", ", input$M_hypergeometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Logistic' && input$lower_tail_logistic == 'lower.tail') {
      funcShaded <- function(x) {
        y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
        y[x > input$x1_logistic] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Logi(", input$location_logistic, ", ", input$scale_logistic, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Logistic' && input$lower_tail_logistic == 'upper.tail') {
      funcShaded <- function(x) {
        y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
        y[x < input$x2_logistic] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Logi(", input$location_logistic, ", ", input$scale_logistic, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Logistic' && input$lower_tail_logistic == 'interval') {
      funcShaded <- function(x) {
        y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
        y[x < input$a_logistic | x > input$b_logistic] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Logi(", input$location_logistic, ", ", input$scale_logistic, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Log-Normal' && input$lower_tail_lognormal == 'lower.tail') {
      funcShaded <- function(x) {
        y <- dlnorm(x,
                    meanlog = input$mean_lognormal,
                    sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )
        y[x > input$x1_lognormal] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(0, qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
        stat_function(fun = dlnorm, args = list(
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Lognormal(", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Log-Normal' && input$lower_tail_lognormal == 'upper.tail') {
      funcShaded <- function(x) {
        y <- dlnorm(x,
                    meanlog = input$mean_lognormal,
                    sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )
        y[x < input$x2_lognormal] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(0, qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
        stat_function(fun = dlnorm, args = list(
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Lognormal(", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Log-Normal' && input$lower_tail_lognormal == 'interval') {
      funcShaded <- function(x) {
        y <- dlnorm(x,
                    meanlog = input$mean_lognormal,
                    sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )
        y[x < input$a_lognormal | x > input$b_lognormal] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(0, qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
        stat_function(fun = dlnorm, args = list(
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Lognormal(", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Negative Binomial (I)' && input$lower_tail_negativebinomial == 'lower.tail') {
      p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
        mutate(Heads = ifelse(heads <= input$x1_negativebinomial, "2", "Other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: NG(", input$r_negativebinomial, ", ", input$p_negativebinomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Negative Binomial (I)' && input$lower_tail_negativebinomial == 'upper.tail') {
      p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
        mutate(Heads = ifelse(heads > input$x2_negativebinomial, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: NG(", input$r_negativebinomial, ", ", input$p_negativebinomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Negative Binomial (I)' && input$lower_tail_negativebinomial == 'interval') {
      p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
        mutate(Heads = ifelse(heads >= input$a_negativebinomial & heads <= input$b_negativebinomial, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: NG(", input$r_negativebinomial, ", ", input$p_negativebinomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Negative Binomial (II)' && input$lower_tail_negativebinomial2 == 'lower.tail') {
      p <- data.frame(heads = input$r_negativebinomial2:(qnbinom(0.999, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE) + input$r_negativebinomial2), prob = dnbinom(x = 0:qnbinom(0.999, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE), size = input$r_negativebinomial2, prob = input$p_negativebinomial2)) %>%
        mutate(Heads = ifelse(heads <= input$x1_negativebinomial2, "2", "Other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: NG(", input$r_negativebinomial2, ", ", input$p_negativebinomial2, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Negative Binomial (II)' && input$lower_tail_negativebinomial2 == 'upper.tail') {
      p <- data.frame(heads = input$r_negativebinomial2:(qnbinom(0.999, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE) + input$r_negativebinomial2), prob = dnbinom(x = 0:qnbinom(0.999, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE), size = input$r_negativebinomial2, prob = input$p_negativebinomial2)) %>%
        mutate(Heads = ifelse(heads > input$x2_negativebinomial2, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: NG(", input$r_negativebinomial2, ", ", input$p_negativebinomial2, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Negative Binomial (II)' && input$lower_tail_negativebinomial2 == 'interval') {
      p <- data.frame(heads = input$r_negativebinomial2:(qnbinom(0.999, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE) + input$r_negativebinomial2), prob = dnbinom(x = 0:qnbinom(0.999, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE), size = input$r_negativebinomial2, prob = input$p_negativebinomial2)) %>%
        mutate(Heads = ifelse(heads >= input$a_negativebinomial2 & heads <= input$b_negativebinomial2, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: NG(", input$r_negativebinomial2, ", ", input$p_negativebinomial2, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Normal' && input$lower_tail_normal == 'lower.tail') {
      funcShaded <- function(x) {
        y <- dnorm(x,
                   mean = input$mean_normal,
                   sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )
        y[x > input$x1_normal] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: N(", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", input$variance_normal, (input$sd_normal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Normal' && input$lower_tail_normal == 'upper.tail') {
      funcShaded <- function(x) {
        y <- dnorm(x,
                   mean = input$mean_normal,
                   sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )
        y[x < input$x2_normal] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: N(", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", input$variance_normal, (input$sd_normal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Normal' && input$lower_tail_normal == 'interval') {
      funcShaded <- function(x) {
        y <- dnorm(x,
                   mean = input$mean_normal,
                   sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )
        y[x < input$a_normal | x > input$b_normal] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: N(", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", input$variance_normal, (input$sd_normal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Poisson' && input$lower_tail_poisson == 'lower.tail') {
      p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
        mutate(Heads = ifelse(heads <= input$x1_poisson, "2", "Other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: Pois(", input$lambda_poisson, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Poisson' && input$lower_tail_poisson == 'upper.tail') {
      p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
        mutate(Heads = ifelse(heads > input$x2_poisson, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: Pois(", input$lambda_poisson, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Poisson' && input$lower_tail_poisson == 'interval') {
      p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
        mutate(Heads = ifelse(heads >= input$a_poisson & heads <= input$b_poisson, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col() +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(input$distribution, " distribution: Pois(", input$lambda_poisson, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Probability mass function") +
        xlab("x")
      p
    } else if (input$distribution == 'Student' && input$lower_tail_student == 'lower.tail') {
      funcShaded <- function(x) {
        y <- dt(x, df = input$df_student)
        y[x > input$x1_student] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = input$df_student)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: St(", input$df_student, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Student' && input$lower_tail_student == 'upper.tail') {
      funcShaded <- function(x) {
        y <- dt(x, df = input$df_student)
        y[x < input$x2_student] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = input$df_student)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: St(", input$df_student, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Student' && input$lower_tail_student == 'interval') {
      funcShaded <- function(x) {
        y <- dt(x, df = input$df_student)
        y[x < input$a_student | x > input$b_student] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = input$df_student)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: St(", input$df_student, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Weibull' && input$lower_tail_weibull == 'lower.tail') {
      funcShaded <- function(x) {
        y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
        y[x > input$x1_weibull] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Weibull(", input$alpha_weibull, ", ", input$beta_weibull, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Weibull' && input$lower_tail_weibull == 'upper.tail') {
      funcShaded <- function(x) {
        y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
        y[x < input$x2_weibull] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Weibull(", input$alpha_weibull, ", ", input$beta_weibull, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$distribution == 'Weibull' && input$lower_tail_weibull == 'interval') {
      funcShaded <- function(x) {
        y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
        y[x < input$a_weibull | x > input$b_weibull] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: Weibull(", input$alpha_weibull, ", ", input$beta_weibull, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    }
    
    return(res)
  })
  
  output$plots <- renderPlot({
    r_plot()
  })
  
  output$parameters_distribution <- renderUI({
    if (input$distribution == "Beta") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\dfrac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)} x^{\\alpha-1} (1 - x)^{\\beta - 1} $$"),
        helpText("where \\( 0 \\leq x \\leq 1, \\alpha > 0, \\beta > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{\\alpha}{\\alpha + \\beta} = \\)", round(input$alpha_beta / (input$alpha_beta + input$beta_beta), 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{\\alpha\\beta}{(\\alpha + \\beta)^2(\\alpha + \\beta+1)}} = \\)", round(sqrt((input$alpha_beta * input$beta_beta) / (((input$alpha_beta + input$beta_beta)^2) * (input$alpha_beta + input$beta_beta + 1))), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{\\alpha\\beta}{(\\alpha + \\beta)^2(\\alpha + \\beta+1)} = \\)", round((input$alpha_beta * input$beta_beta) / (((input$alpha_beta + input$beta_beta)^2) * (input$alpha_beta + input$beta_beta + 1)), 3))
      )
    } else if (input$distribution == "Binomial") {
      withMathJax(
        helpText("Probability mass function: $$ p(x) = P(X = x) = \\binom{n}{x}p^x(1-p)^{n-x}$$ "),
        helpText("where \\( x = 0, 1, \\dots, n\\) and \\( 0 \\leq p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = np = \\)", round(input$n_binomial * input$p_binomial, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{np(1-p)} = \\)", round(sqrt(input$n_binomial * input$p_binomial * (1 - input$p_binomial)), 3)),
        helpText("\\(\\sigma^2 = Var(X) = np(1-p) = \\)", round(input$n_binomial * input$p_binomial * (1 - input$p_binomial), 3))
      )
    } else if (input$distribution == "Cauchy") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\dfrac{1}{\\pi\\gamma \\Big[ 1 + \\big(\\dfrac{x - x_0}{\\gamma}\\big)^2\\Big]} $$"),
        helpText("where \\( -\\infty < x_0 < \\infty, -\\infty < x < \\infty, y > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = Undefined\\)"),
        helpText("\\(\\sigma = SD(X) = Undefined\\)"),
        helpText("\\(\\sigma^2 = Var(X) = Undefined \\)"),
        helpText("\\(median = x_0 = \\)", round(input$location_cauchy, 3)),
        helpText("\\(mode = x_0 = \\)", round(input$location_cauchy, 3))
      )
    } else if (input$distribution == "Chi-square") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\dfrac{1}{2^{df/2}\\Gamma\\big(\\dfrac{df}{2}\\big)} x^{df/2 - 1} e^{-x/2} $$"),
        helpText("where \\( x > 0, df > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = df = \\)", round(input$df_chisquare, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{2df} = \\)", round(sqrt(2 * input$df_chisquare), 3)),
        helpText("\\(\\sigma^2 = Var(X) = 2df = \\)", round(2 * input$df_chisquare, 3))
      )
    } else if (input$distribution == "Exponential") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\lambda e^{-\\lambda x} $$"),
        helpText("where \\( x > 0, \\lambda > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{1}{\\lambda} = \\)", round(1 / input$rate_exponential, 3)),
        helpText("\\(\\sigma = SD(X) = \\dfrac{1}{\\lambda} = \\)", round(1 / input$rate_exponential, 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{1}{\\lambda^2} = \\)", round(1 / (input$rate_exponential^2), 3))
      )
    } else if (input$distribution == "Fisher") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\dfrac{\\Gamma\\big(\\dfrac{df_1 + df_2}{2}\\big) \\big(\\dfrac{df_1}{df_2}\\big)^{\\dfrac{df_1}{2}}x^{\\dfrac{df_1}{2}-1}}{\\Gamma\\big(\\dfrac{df_1}{2}\\big)\\Gamma\\big(\\dfrac{df_2}{2}\\big)\\big(1 + \\dfrac{df_1 x}{df_2}\\big)^{\\dfrac{df_1 + df_2}{2}}} $$"),
        helpText("where \\( df_1, df_2 > 0, x \\geq 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{df_2}{df_2 - 2} = \\)", ifelse(input$df2_fisher > 2, round(input$df2_fisher / (input$df2_fisher - 2), 3), "Undefined")),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{2df^2_2(df_1 + df_2 - 2)}{df_1(df_2 - 2)^2(df_2 - 4)}} = \\)", ifelse(input$df2_fisher > 4, round(sqrt((2 * input$df2_fisher^2 * (input$df1_fisher + input$df2_fisher - 2)) / (input$df1_fisher * (input$df2_fisher - 2)^2 * (input$df2_fisher - 4))), 3), "Undefined")),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{2df^2_2(df_1 + df_2 - 2)}{df_1(df_2 - 2)^2(df_2 - 4)} = \\)", ifelse(input$df2_fisher > 4, round((2 * input$df2_fisher^2 * (input$df1_fisher + input$df2_fisher - 2)) / (input$df1_fisher * (input$df2_fisher - 2)^2 * (input$df2_fisher - 4)), 3), "Undefined"))
      )
    } else if (input$distribution == "Gamma") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\dfrac{\\beta^\\alpha}{\\Gamma(\\alpha)} x^{\\alpha -1}e^{-\\beta x} $$"),
        helpText("where \\( x > 0, \\alpha > 0, \\beta > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{\\alpha}{\\beta} = \\)", round(input$alpha_gamma / input$beta_gamma, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{\\alpha}{\\beta^2}} = \\)", round(sqrt(input$alpha_gamma / (input$beta_gamma^2)), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{\\alpha}{\\beta^2} = \\)", round(input$alpha_gamma / (input$beta_gamma^2), 3))
      )
    } else if (input$distribution == "Geometric (I)") {
      withMathJax(
        helpText("Probability mass function: $$ p(x) = P(X = x) = (1 - p)^x p $$"),
        helpText("where \\( x = 0, 1, 2, \\dots \\) and \\( 0 < p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{1-p}{p} = \\)", round((1 - input$p_geometric) / input$p_geometric, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{1-p}{p^2}} = \\)", round(sqrt((1 - input$p_geometric) / (input$p_geometric^2)), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{1-p}{p^2} = \\)", round((1 - input$p_geometric) / (input$p_geometric^2), 3))
      )
    } else if (input$distribution == "Geometric (II)") {
      withMathJax(
        helpText("Probability mass function: $$ p(x) = P(X = x) = (1 - p)^{x-1} p $$"),
        helpText("where \\( x = 1, 2, \\dots \\) and \\( 0 < p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{1}{p} = \\)", round((1) / input$p_geometric2, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{1-p}{p^2}} = \\)", round(sqrt((1 - input$p_geometric2) / (input$p_geometric2^2)), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{1-p}{p^2} = \\)", round((1 - input$p_geometric2) / (input$p_geometric2^2), 3))
      )
    } else if (input$distribution == "Hypergeometric") {
      withMathJax(
        helpText("Probability mass function: $$ p(x) = P(X = x) = \\dfrac{\\binom{M}{x} \\binom{N-M}{n-x}}{\\binom{N}{n}}  $$"),
        helpText("for \\( x = 0, 1, \\dots , n\\)"),
        helpText("where \\( x \\leq M \\) and \\( n - x \\leq N - M \\)"),
        br(),
        helpText("\\(\\mu = E(X) = n\\dfrac{M}{N} = \\)", round(input$n_hypergeometric * (input$M_hypergeometric / input$N_hypergeometric), 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{n\\dfrac{M}{N}\\Big(1 - \\dfrac{M}{N}\\Big)\\Big(\\dfrac{N-n}{N-1}\\Big)} = \\)", round(sqrt(input$n_hypergeometric * input$M_hypergeometric / input$N_hypergeometric * (1 - (input$M_hypergeometric / input$N_hypergeometric)) * ((input$N_hypergeometric - input$n_hypergeometric) / (input$N_hypergeometric - 1))), 3)),
        helpText("\\(\\sigma^2 = Var(X) = n\\dfrac{M}{N}\\Big(1 - \\dfrac{M}{N}\\Big)\\Big(\\dfrac{N-n}{N-1}\\Big) = \\)", round(input$n_hypergeometric * input$M_hypergeometric / input$N_hypergeometric * (1 - (input$M_hypergeometric / input$N_hypergeometric)) * ((input$N_hypergeometric - input$n_hypergeometric) / (input$N_hypergeometric - 1)), 3))
      )
    } else if (input$distribution == "Logistic") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\dfrac{e^{-\\dfrac{x-\\mu}{s}}}{s\\Bigg(1 + e^{-\\dfrac{x - \\mu}{s}}\\Bigg)^2} $$"),
        helpText("where \\( -\\infty < x < \\infty, -\\infty < \\mu < \\infty, s > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\mu = \\)", round(input$location_logistic, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{s^2\\pi^2}{3}} = \\)", round(sqrt(((input$scale_logistic^2) * (pi^2)) / 3), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{s^2\\pi^2}{3} = \\)", round(((input$scale_logistic^2) * (pi^2)) / 3, 3))
      )
    } else if (input$distribution == "Log-Normal") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\dfrac{1}{x\\sqrt{2\\pi \\sigma^2}}e^{-\\dfrac{1}{2\\sigma^2}(ln(x)-\\mu)^2} $$"),
        helpText("where \\( x > 0, -\\infty < \\mu < \\infty, \\sigma > 0\\)"),
        br(),
        helpText("\\(E(X) = e^{\\mu + \\dfrac{\\sigma^2}{2}} = \\)", round(exp(input$mean_lognormal + ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal / 2, (input$sd_lognormal^2) / 2)), 3)),
        helpText("\\(SD(X) = \\sqrt{(e^{\\sigma^2} - 1)e^{2\\mu + \\sigma^2}} = \\)", round(sqrt((exp(ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2))) - 1) * exp((2 * input$mean_lognormal) + ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)))), 3)),
        helpText("\\(Var(X) = (e^{\\sigma^2} - 1)e^{2\\mu + \\sigma^2} = \\)", round((exp(ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2))) - 1) * exp((2 * input$mean_lognormal) + ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2))), 3))
      )
    } else if (input$distribution == "Negative Binomial (I)") {
      withMathJax(
        helpText("Probability mass function: $$ p(x) = P(X = x) = \\binom{x+r-1}{r-1} (1-p)^x p^r $$"),
        helpText("where \\( x = 0, 1, 2, \\dots, r = 1, 2, \\dots \\) and \\( 0 < p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{r(1-p)}{p} = \\)", round((input$r_negativebinomial * (1 - input$p_negativebinomial) / input$p_negativebinomial), 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{r(1-p)}{p^2}} = \\)", round(sqrt((input$r_negativebinomial * (1 - input$p_negativebinomial) / (input$p_negativebinomial^2))), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{r(1-p)}{p^2} = \\)", round((input$r_negativebinomial * (1 - input$p_negativebinomial) / (input$p_negativebinomial^2)), 3))
      )
    } else if (input$distribution == "Negative Binomial (II)") {
      withMathJax(
        helpText("Probability mass function: $$ p(x) = P(X = x) = \\binom{x-1}{r-1}p^r (1-p)^{x-r} $$"),
        helpText("where \\( x = r, r+1, \\dots \\) and \\( 0 < p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{r}{p} = \\)", round((input$r_negativebinomial2 / input$p_negativebinomial2), 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{r(1-p)}{p^2}} = \\)", round(sqrt((input$r_negativebinomial2 * (1 - input$p_negativebinomial2) / (input$p_negativebinomial2^2))), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{r(1-p)}{p^2} = \\)", round((input$r_negativebinomial2 * (1 - input$p_negativebinomial2) / (input$p_negativebinomial2^2)), 3))
      )
    } else if (input$distribution == "Normal") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\dfrac{1}{\\sqrt{2\\pi \\sigma^2}}e^{-\\dfrac{1}{2\\sigma^2}(x-\\mu)^2} $$"),
        helpText("where \\( -\\infty < x < \\infty, -\\infty < \\mu < \\infty, \\sigma > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\)", round(input$mean_normal, 3)),
        helpText("\\(\\sigma = SD(X) = \\)", ifelse(input$variance_sd == "variance_true", round(sqrt(input$variance_normal), 3), round(input$sd_normal, 3))),
        helpText("\\(\\sigma^2 = Var(X) = \\)", ifelse(input$variance_sd == "variance_true", round(input$variance_normal, 3), round(input$sd_normal^2, 3)))
      )
    } else if (input$distribution == "Poisson") {
      withMathJax(
        helpText("Probability mass function: $$ p(x) = P(X = x) = \\dfrac{e^{-\\lambda}\\lambda^x}{x!} $$"),
        helpText("for \\( x = 0, 1, 2, \\dots\\)"),
        helpText("where \\( \\lambda > 0 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\lambda = \\)", round(input$lambda_poisson, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\lambda} = \\)", round(sqrt(input$lambda_poisson), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\lambda = \\)", round(input$lambda_poisson, 3))
      )
    } else if (input$distribution == "Student") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\dfrac{\\Gamma \\big(\\dfrac{df+1}{2}\\big)}{\\sqrt{df \\pi} \\Gamma \\big(\\dfrac{df}{2}\\big)} \\Big(1 + \\dfrac{x^2}{df}\\Big)^{-\\dfrac{df+1}{2}}$$"),
        helpText("where \\( -\\infty < x < \\infty, df > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\)", ifelse(input$df_student > 1, 0, "Undefined")),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{df}{df - 2}} = \\)", ifelse(input$df_student > 2, round(sqrt(input$df_student / (input$df_student - 2)), 3), "Undefined")),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{df}{df-2} = \\)", ifelse(input$df_student > 2, round(input$df_student / (input$df_student - 2), 3), "Undefined"))
      )
    } else if (input$distribution == "Weibull") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\dfrac{\\alpha}{\\beta} \\big(\\dfrac{x}{\\beta}\\big)^{\\alpha-1} e^{-(x / \\beta)^\\alpha} $$"),
        helpText("where \\( x > 0, \\alpha >0, \\beta > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\beta\\Gamma\\big(1 + \\dfrac{1}{\\alpha}\\big) = \\)", round(weibullparinv(shape = input$alpha_weibull, scale = input$beta_weibull, loc = 0)$mu, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\beta^2\\Big(\\Gamma\\big(1 + \\dfrac{2}{\\alpha}\\big) - \\Gamma\\big(1 + \\dfrac{1}{\\alpha}\\big)^2\\Big)} = \\)", round(weibullparinv(shape = input$alpha_weibull, scale = input$beta_weibull, loc = 0)$sigma, 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\beta^2\\Big(\\Gamma\\big(1 + \\dfrac{2}{\\alpha}\\big) - \\Gamma\\big(1 + \\dfrac{1}{\\alpha}\\big)^2\\Big) = \\)", round(weibullparinv(shape = input$alpha_weibull, scale = input$beta_weibull, loc = 0)$sigma^2, 3))
      )
    } else {
      print("loading...")
    }
  })
  
}
