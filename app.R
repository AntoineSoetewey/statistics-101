#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(mixdist)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Statistics 101 - Probability distributions"),
   h4(tags$a(href="https://www.antoinesoetewey.com/", "Antoine Soetewey")),
   withMathJax(),
   
   # tabsetPanel(
     # Create tab1
     # tabPanel(
     #   title = "Distributions",
   
   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "distribution",
          label = "Distribution:",
          choices = c("Beta", "Binomial", "Cauchy", "Chi-square", "Exponential", "Fisher", "Gamma", "Geometric (I)", "Geometric (II)", "Hypergeometric", "Logistic", "Log-Normal", "Negative Binomial (I)", "Negative Binomial (II)", "Normal", "Poisson", "Student", "Weibull"),
          multiple = FALSE,
          selected = "Normal"
        ),
        hr(),
        # tags$b("Parameter(s)"),
        conditionalPanel(
          condition = "input.distribution == 'Beta'",
          numericInput("alpha_beta", "Shape \\(\\alpha\\):",
                       value = 1, min = 0, step = 1),
          numericInput("beta_beta", "Shape \\(\\beta\\):",
                       value = 3, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Binomial'",
          numericInput("n_binomial", "Number of trials \\(n\\):",
                       value = 20, min = 0, step = 1),
          numericInput("p_binomial", "Probability of success \\(p\\):",
                       value = 0.5, min = 0, max = 1, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Cauchy'",
          numericInput("location_cauchy", "Location \\(x_0\\):",
                       value = 0, step = 1),
          numericInput("scale_cauchy", "Scale \\(\\gamma\\):",
                       value = 1, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Chi-square'",
          numericInput("df_chisquare", "Degrees of freedom \\(df\\):",
                       value = 6, min = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Exponential'",
          numericInput("rate_exponential", "Rate \\(\\lambda\\):",
                       value = 1, min = 0, step = 0.5)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Fisher'",
          numericInput("df1_fisher", "Degrees of freedom \\(df_1\\):",
                       value = 10, min = 1, step = 1),
          numericInput("df2_fisher", "Degrees of freedom \\(df_2\\):",
                       value = 5, min = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Gamma'",
          numericInput("alpha_gamma", "Shape \\(\\alpha\\):",
                       value = 3, min = 0, step = 1),
          numericInput("beta_gamma", "Rate \\(\\beta\\):",
                       value = 2, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (I)'",
          numericInput("p_geometric", "Probability of success \\(p\\):",
                       value = 0.5, min = 0, max = 1, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (II)'",
          numericInput("p_geometric2", "Probability of success \\(p\\):",
                       value = 0.5, min = 0, max = 1, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Hypergeometric'",
          numericInput("n_hypergeometric", "Sample size \\(n\\):",
                       value = 100, min = 0, step = 1),
          numericInput("N_hypergeometric", "Total number of objects \\(N\\):",
                       value = 500, min = 0, step = 1),
          numericInput("M_hypergeometric", "Number of successes \\(M\\):",
                       value = 50, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Logistic'",
          numericInput("location_logistic", "Location \\(\\mu\\):",
                       value = 0, step = 1),
          numericInput("scale_logistic", "Scale \\(s\\):",
                       value = 1, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Log-Normal'",
          numericInput("mean_lognormal", "Mean \\(\\mu\\):",
                       value = 0, step = 1),
          radioButtons(
            inputId = "variance_sd_lognormal",
            label = NULL,
            choices = c(
              "Variance \\(\\sigma^2\\)" = "variance_true",
              "Standard deviation \\(\\sigma\\)" = "variance_false"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Log-Normal' && input.variance_sd_lognormal == 'variance_true'",
          numericInput("variance_lognormal", "Variance \\(\\sigma^2\\):",
                       value = 1, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Log-Normal' && input.variance_sd_lognormal == 'variance_false'",
          numericInput("sd_lognormal", "Standard deviation \\(\\sigma\\):",
                       value = 1, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (I)'",
          numericInput("r_negativebinomial", "Number of successes \\(r\\):",
                       value = 5, min = 1, step = 1),
          numericInput("p_negativebinomial", "Probability of success \\(p\\):",
                       value = 0.5, min = 0, max = 1, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (II)'",
          numericInput("r_negativebinomial2", "Number of successes \\(r\\):",
                       value = 5, min = 1, step = 1),
          numericInput("p_negativebinomial2", "Probability of success \\(p\\):",
                       value = 0.5, min = 0, max = 1, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Normal'",
          numericInput("mean_normal", "Mean \\(\\mu\\):",
                       value = 0, step = 1),
          radioButtons(
            inputId = "variance_sd",
            label = NULL,
            choices = c(
              "Variance \\(\\sigma^2\\)" = "variance_true",
              "Standard deviation \\(\\sigma\\)" = "variance_false"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Normal' && input.variance_sd == 'variance_true'",
          numericInput("variance_normal", "Variance \\(\\sigma^2\\):",
                       value = 1, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Normal' && input.variance_sd == 'variance_false'",
          numericInput("sd_normal", "Standard deviation \\(\\sigma\\):",
                       value = 1, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Poisson'",
          numericInput("lambda_poisson", "Rate \\(\\lambda\\):",
                       value = 4, min = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Student'",
          numericInput("df_student", "Degrees of freedom \\(df\\):",
                       value = 10, min = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Weibull'",
          numericInput("alpha_weibull", "Shape \\(\\alpha\\):",
                       value = 5, min = 0, step = 1),
          numericInput("beta_weibull", "Scale \\(\\beta\\):",
                       value = 1, min = 0, step = 1)
        ),
        hr(),
        conditionalPanel(
          condition = "input.distribution == 'Beta'",
          radioButtons(
            inputId = "lower_tail_beta",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Binomial'",
          radioButtons(
            inputId = "lower_tail_binomial",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Cauchy'",
          radioButtons(
            inputId = "lower_tail_cauchy",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Chi-square'",
          radioButtons(
            inputId = "lower_tail_chisquare",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Exponential'",
          radioButtons(
            inputId = "lower_tail_exponential",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Fisher'",
          radioButtons(
            inputId = "lower_tail_fisher",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Gamma'",
          radioButtons(
            inputId = "lower_tail_gamma",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (I)'",
          radioButtons(
            inputId = "lower_tail_geometric",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (II)'",
          radioButtons(
            inputId = "lower_tail_geometric2",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Hypergeometric'",
          radioButtons(
            inputId = "lower_tail_hypergeometric",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Logistic'",
          radioButtons(
            inputId = "lower_tail_logistic",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Log-Normal'",
          radioButtons(
            inputId = "lower_tail_lognormal",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (I)'",
          radioButtons(
            inputId = "lower_tail_negativebinomial",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (II)'",
          radioButtons(
            inputId = "lower_tail_negativebinomial2",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Normal'",
          radioButtons(
            inputId = "lower_tail_normal",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Poisson'",
          radioButtons(
            inputId = "lower_tail_poisson",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Student'",
          radioButtons(
            inputId = "lower_tail_student",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        conditionalPanel(
          condition = "input.distribution == 'Weibull'",
          radioButtons(
            inputId = "lower_tail_weibull",
            label = NULL,
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          )
        ),
        hr(),
        conditionalPanel(
          condition = "input.distribution == 'Beta' && input.lower_tail_beta == 'lower.tail'",
          numericInput("x1_beta", "x:",
                       value = 0.45, min = 0, max = 1, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Beta' && input.lower_tail_beta == 'upper.tail'",
          numericInput("x2_beta", "x:",
                       value = 0.45, min = 0, max = 1, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Beta' && input.lower_tail_beta == 'interval'",
          numericInput("a_beta", "a:",
                       value = 0.25, min = 0, max = 1, step = 0.01),
          numericInput("b_beta", "b: \\( (a \\leq b) \\)",
                       value = 0.45, min = 0, max = 1, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Binomial' && input.lower_tail_binomial == 'lower.tail'",
          numericInput("x1_binomial", "x:",
                       value = 8, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Binomial' && input.lower_tail_binomial == 'upper.tail'",
          numericInput("x2_binomial", "x:",
                       value = 8, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Binomial' && input.lower_tail_binomial == 'interval'",
          numericInput("a_binomial", "a:",
                       value = 8, min = 0, step = 1),
          numericInput("b_binomial", "b: \\( (a \\leq b) \\)",
                       value = 12, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Cauchy' && input.lower_tail_cauchy == 'lower.tail'",
          numericInput("x1_cauchy", "x:",
                       value = 1.2, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Cauchy' && input.lower_tail_cauchy == 'upper.tail'",
          numericInput("x2_cauchy", "x:",
                       value = 1.2, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Cauchy' && input.lower_tail_cauchy == 'interval'",
          numericInput("a_cauchy", "a:",
                       value = -1.2, step = 1),
          numericInput("b_cauchy", "b: \\( (a \\leq b) \\)",
                       value = 1.2, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Chi-square' && input.lower_tail_chisquare == 'lower.tail'",
          numericInput("x1_chisquare", "x:",
                       value = 9.6, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Chi-square' && input.lower_tail_chisquare == 'upper.tail'",
          numericInput("x2_chisquare", "x:",
                       value = 9.6, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Chi-square' && input.lower_tail_chisquare == 'interval'",
          numericInput("a_chisquare", "a:",
                       value = 9.6, min = 0, step = 1),
          numericInput("b_chisquare", "b: \\( (a \\leq b) \\)",
                       value = 14.4, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Exponential' && input.lower_tail_exponential == 'lower.tail'",
          numericInput("x1_exponential", "x:",
                       value = 2.24, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Exponential' && input.lower_tail_exponential == 'upper.tail'",
          numericInput("x2_exponential", "x:",
                       value = 2.24, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Exponential' && input.lower_tail_exponential == 'interval'",
          numericInput("a_exponential", "a:",
                       value = 2.24, min = 0, step = 1),
          numericInput("b_exponential", "b: \\( (a \\leq b) \\)",
                       value = 3.36, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Fisher' && input.lower_tail_fisher == 'lower.tail'",
          numericInput("x1_fisher", "x:",
                       value = 4.14, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Fisher' && input.lower_tail_fisher == 'upper.tail'",
          numericInput("x2_fisher", "x:",
                       value = 4.14, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Fisher' && input.lower_tail_fisher == 'interval'",
          numericInput("a_fisher", "a:",
                       value = 2.76, min = 0, step = 1),
          numericInput("b_fisher", "b: \\( (a \\leq b) \\)",
                       value = 4.14, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Gamma' && input.lower_tail_gamma == 'lower.tail'",
          numericInput("x1_gamma", "x:",
                       value = 2.4, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Gamma' && input.lower_tail_gamma == 'upper.tail'",
          numericInput("x2_gamma", "x:",
                       value = 2.4, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Gamma' && input.lower_tail_gamma == 'interval'",
          numericInput("a_gamma", "a:",
                       value = 0.8, min = 0, step = 1),
          numericInput("b_gamma", "b: \\( (a \\leq b) \\)",
                       value = 2.4, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (I)' && input.lower_tail_geometric == 'lower.tail'",
          helpText("Number of failures before the \\(1^{st}\\) success"),
          numericInput("x1_geometric", "x:",
                       value = 1, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (I)' && input.lower_tail_geometric == 'upper.tail'",
          helpText("Number of failures before the \\(1^{st}\\) success"),
          numericInput("x2_geometric", "x:",
                       value = 1, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (I)' && input.lower_tail_geometric == 'interval'",
          helpText("Number of failures before the \\(1^{st}\\) success"),
          numericInput("a_geometric", "a:",
                       value = 1, min = 0, step = 1),
          numericInput("b_geometric", "b: \\( (a \\leq b) \\)",
                       value = 3, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (II)' && input.lower_tail_geometric2 == 'lower.tail'",
          helpText("The trial on which the \\(1^{st}\\) success occurs"),
          numericInput("x1_geometric2", "x:",
                       value = 2, min = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (II)' && input.lower_tail_geometric2 == 'upper.tail'",
          helpText("The trial on which the \\(1^{st}\\) success occurs"),
          numericInput("x2_geometric2", "x:",
                       value = 2, min = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (II)' && input.lower_tail_geometric2 == 'interval'",
          helpText("The trial on which the \\(1^{st}\\) success occurs"),
          numericInput("a_geometric2", "a:",
                       value = 2, min = 1, step = 1),
          numericInput("b_geometric2", "b: \\( (a \\leq b) \\)",
                       value = 4, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Hypergeometric' && input.lower_tail_hypergeometric == 'lower.tail'",
          numericInput("x1_hypergeometric", "x:",
                       value = 8, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Hypergeometric' && input.lower_tail_hypergeometric == 'upper.tail'",
          numericInput("x2_hypergeometric", "x:",
                       value = 8, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Hypergeometric' && input.lower_tail_hypergeometric == 'interval'",
          numericInput("a_hypergeometric", "a:",
                       value = 8, min = 0, step = 1),
          numericInput("b_hypergeometric", "b: \\( (a \\leq b) \\)",
                       value = 12, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Logistic' && input.lower_tail_logistic == 'lower.tail'",
          numericInput("x1_logistic", "x:",
                       value = 1.2, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Logistic' && input.lower_tail_logistic == 'upper.tail'",
          numericInput("x2_logistic", "x:",
                       value = 1.2, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Logistic' && input.lower_tail_logistic == 'interval'",
          numericInput("a_logistic", "a:",
                       value = -1.2, step = 1),
          numericInput("b_logistic", "b: \\( (a \\leq b) \\)",
                       value = 1.2, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Log-Normal' && input.lower_tail_lognormal == 'lower.tail'",
          numericInput("x1_lognormal", "x:",
                       value = 1, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Log-Normal' && input.lower_tail_lognormal == 'upper.tail'",
          numericInput("x2_lognormal", "x:",
                       value = 1, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Log-Normal' && input.lower_tail_lognormal == 'interval'",
          numericInput("a_lognormal", "a:",
                       value = 1, min = 0, step = 1),
          numericInput("b_lognormal", "b: \\( (a \\leq b) \\)",
                       value = 2, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (I)' && input.lower_tail_negativebinomial == 'lower.tail'",
          helpText("Number of failures before the \\(r^{th}\\) success"),
          numericInput("x1_negativebinomial", "x:",
                       value = 2, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (I)' && input.lower_tail_negativebinomial == 'upper.tail'",
          helpText("Number of failures before the \\(r^{th}\\) success"),
          numericInput("x2_negativebinomial", "x:",
                       value = 2, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (I)' && input.lower_tail_negativebinomial == 'interval'",
          helpText("Number of failures before the \\(r^{th}\\) success"),
          numericInput("a_negativebinomial", "a:",
                       value = 2, min = 0, step = 1),
          numericInput("b_negativebinomial", "b: \\( (a \\leq b) \\)",
                       value = 4, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (II)' && input.lower_tail_negativebinomial2 == 'lower.tail'",
          helpText("The trial on which the \\(r^{th}\\) success occurs"),
          numericInput("x1_negativebinomial2", "x: \\( (x \\geq r) \\)",
                       value = 7, min = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (II)' && input.lower_tail_negativebinomial2 == 'upper.tail'",
          helpText("The trial on which the \\(r^{th}\\) success occurs"),
          numericInput("x2_negativebinomial2", "x: \\( (x \\geq r) \\)",
                       value = 7, min = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (II)' && input.lower_tail_negativebinomial2 == 'interval'",
          helpText("The trial on which the \\(r^{th}\\) success occurs"),
          numericInput("a_negativebinomial2", "a: \\( (a \\geq r) \\)",
                       value = 7, min = 1, step = 1),
          numericInput("b_negativebinomial2", "b: \\( (r \\leq a \\leq b) \\)",
                       value = 9, min = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Normal' && input.lower_tail_normal == 'lower.tail'",
          numericInput("x1_normal", "x:",
                       value = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Normal' && input.lower_tail_normal == 'upper.tail'",
          numericInput("x2_normal", "x:",
                       value = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Normal' && input.lower_tail_normal == 'interval'",
          numericInput("a_normal", "a:",
                       value = -1, step = 1),
          numericInput("b_normal", "b: \\( (a \\leq b) \\)",
                       value = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Poisson' && input.lower_tail_poisson == 'lower.tail'",
          numericInput("x1_poisson", "x:",
                       value = 6, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Poisson' && input.lower_tail_poisson == 'upper.tail'",
          numericInput("x2_poisson", "x:",
                       value = 6, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Poisson' && input.lower_tail_poisson == 'interval'",
          numericInput("a_poisson", "a:",
                       value = 6, min = 0, step = 1),
          numericInput("b_poisson", "b: \\( (a \\leq b) \\)",
                       value = 10, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Student' && input.lower_tail_student == 'lower.tail'",
          numericInput("x1_student", "x:",
                       value = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Student' && input.lower_tail_student == 'upper.tail'",
          numericInput("x2_student", "x:",
                       value = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Student' && input.lower_tail_student == 'interval'",
          numericInput("a_student", "a:",
                       value = -1, step = 1),
          numericInput("b_student", "b: \\( (a \\leq b) \\)",
                       value = 1, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Weibull' && input.lower_tail_weibull == 'lower.tail'",
          numericInput("x1_weibull", "x:",
                       value = 0.8, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Weibull' && input.lower_tail_weibull == 'upper.tail'",
          numericInput("x2_weibull", "x:",
                       value = 0.8, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Weibull' && input.lower_tail_weibull == 'interval'",
          numericInput("a_weibull", "a:",
                       value = 0.8, min = 0, step = 1),
          numericInput("b_weibull", "b: \\( (a \\leq b) \\)",
                       value = 1.2, min = 0, step = 1)
        ),
        hr(),
        HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/statistics-101/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/statistics-101/blob/master/app.R">code</a>. Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a>.</p>')
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        br(),
        tags$b("Solution:"),
        uiOutput("results_distribution"),
        br(),
        conditionalPanel(
          condition = "input.distribution == 'Beta' && input.lower_tail_beta == 'lower.tail'",
          plotOutput("betaPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Beta' && input.lower_tail_beta == 'upper.tail'",
          plotOutput("betaPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Beta' && input.lower_tail_beta == 'interval'",
          plotOutput("betaPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Binomial' && input.lower_tail_binomial == 'lower.tail'",
          plotOutput("binomialPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Binomial' && input.lower_tail_binomial == 'upper.tail'",
          plotOutput("binomialPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Binomial' && input.lower_tail_binomial == 'interval'",
          plotOutput("binomialPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Cauchy' && input.lower_tail_cauchy == 'lower.tail'",
          plotOutput("cauchyPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Cauchy' && input.lower_tail_cauchy == 'upper.tail'",
          plotOutput("cauchyPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Cauchy' && input.lower_tail_cauchy == 'interval'",
          plotOutput("cauchyPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Chi-square' && input.lower_tail_chisquare == 'lower.tail'",
          plotOutput("chisquarePlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Chi-square' && input.lower_tail_chisquare == 'upper.tail'",
          plotOutput("chisquarePlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Chi-square' && input.lower_tail_chisquare == 'interval'",
          plotOutput("chisquarePlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Exponential' && input.lower_tail_exponential == 'lower.tail'",
          plotOutput("exponentialPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Exponential' && input.lower_tail_exponential == 'upper.tail'",
          plotOutput("exponentialPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Exponential' && input.lower_tail_exponential == 'interval'",
          plotOutput("exponentialPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Fisher' && input.lower_tail_fisher == 'lower.tail'",
          plotOutput("fisherPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Fisher' && input.lower_tail_fisher == 'upper.tail'",
          plotOutput("fisherPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Fisher' && input.lower_tail_fisher == 'interval'",
          plotOutput("fisherPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Gamma' && input.lower_tail_gamma == 'lower.tail'",
          plotOutput("gammaPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Gamma' && input.lower_tail_gamma == 'upper.tail'",
          plotOutput("gammaPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Gamma' && input.lower_tail_gamma == 'interval'",
          plotOutput("gammaPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (I)' && input.lower_tail_geometric == 'lower.tail'",
          plotOutput("geometricPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (I)' && input.lower_tail_geometric == 'upper.tail'",
          plotOutput("geometricPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (I)' && input.lower_tail_geometric == 'interval'",
          plotOutput("geometricPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (II)' && input.lower_tail_geometric2 == 'lower.tail'",
          plotOutput("geometric2Plot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (II)' && input.lower_tail_geometric2 == 'upper.tail'",
          plotOutput("geometric2Plot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Geometric (II)' && input.lower_tail_geometric2 == 'interval'",
          plotOutput("geometric2Plot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Hypergeometric' && input.lower_tail_hypergeometric == 'lower.tail'",
          plotOutput("hypergeometricPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Hypergeometric' && input.lower_tail_hypergeometric == 'upper.tail'",
          plotOutput("hypergeometricPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Hypergeometric' && input.lower_tail_hypergeometric == 'interval'",
          plotOutput("hypergeometricPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Logistic' && input.lower_tail_logistic == 'lower.tail'",
          plotOutput("logisticPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Logistic' && input.lower_tail_logistic == 'upper.tail'",
          plotOutput("logisticPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Logistic' && input.lower_tail_logistic == 'interval'",
          plotOutput("logisticPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Log-Normal' && input.lower_tail_lognormal == 'lower.tail'",
          plotOutput("lognormalPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Log-Normal' && input.lower_tail_lognormal == 'upper.tail'",
          plotOutput("lognormalPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Log-Normal' && input.lower_tail_lognormal == 'interval'",
          plotOutput("lognormalPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (I)' && input.lower_tail_negativebinomial == 'lower.tail'",
          plotOutput("negativebinomialPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (I)' && input.lower_tail_negativebinomial == 'upper.tail'",
          plotOutput("negativebinomialPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (I)' && input.lower_tail_negativebinomial == 'interval'",
          plotOutput("negativebinomialPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (II)' && input.lower_tail_negativebinomial2 == 'lower.tail'",
          plotOutput("negativebinomial2Plot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (II)' && input.lower_tail_negativebinomial2 == 'upper.tail'",
          plotOutput("negativebinomial2Plot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Negative Binomial (II)' && input.lower_tail_negativebinomial2 == 'interval'",
          plotOutput("negativebinomial2Plot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Normal' && input.lower_tail_normal == 'lower.tail'",
          plotOutput("normalPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Normal' && input.lower_tail_normal == 'upper.tail'",
          plotOutput("normalPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Normal' && input.lower_tail_normal == 'interval'",
          plotOutput("normalPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Poisson' && input.lower_tail_poisson == 'lower.tail'",
          plotOutput("poissonPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Poisson' && input.lower_tail_poisson == 'upper.tail'",
          plotOutput("poissonPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Poisson' && input.lower_tail_poisson == 'interval'",
          plotOutput("poissonPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Student' && input.lower_tail_student == 'lower.tail'",
          plotOutput("studentPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Student' && input.lower_tail_student == 'upper.tail'",
          plotOutput("studentPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Student' && input.lower_tail_student == 'interval'",
          plotOutput("studentPlot_interval")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Weibull' && input.lower_tail_weibull == 'lower.tail'",
          plotOutput("weibullPlot_lower")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Weibull' && input.lower_tail_weibull == 'upper.tail'",
          plotOutput("weibullPlot_upper")
        ),
        conditionalPanel(
          condition = "input.distribution == 'Weibull' && input.lower_tail_weibull == 'interval'",
          plotOutput("weibullPlot_interval")
        ),
        br(),
        tags$b("Details:"),
        br(),
        uiOutput("parameters_distribution"),
        # br(),
        # br(),
        # tags$a(href="https://www.antoinesoetewey.com/", "Back to www.antoinesoetewey.com"),
        br(),
        br()
      )
   # )
# )
# ,
# tabPanel(
#   title = "Statistical tests",
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout(
#     
#     # Sidebar to demonstrate various slider options ----
#     sidebarPanel(
#       
#       # Input: Simple integer interval ----
#       selectInput(
#         inputId = "test",
#         label = "Test:",
#         choices = c("Binomial", "Chi-square", "Fisher", "Normal", "Poisson", "Student"),
#         multiple = FALSE,
#         selected = "Normal"
#       ),
#       hr(),
#       HTML('<p>Report a bug or request the code <a href="https://www.antoinesoetewey.com/contact/">here</a>.</p>')
#     ),
#     
#     # Main panel for displaying outputs ----
#     mainPanel(
#       br(),
#       textOutput("results_tests"),
#       br(),
#       tags$a(href="https://www.antoinesoetewey.com/", "Back to www.antoinesoetewey.com"),
#       br(),
#       br()
#     )
#   )
# )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$results_distribution <- renderUI({
    if (input$distribution == "Beta") {
      withMathJax(
        paste0("\\(X \\sim Beta(\\alpha = \\)", " ", input$alpha_beta, ", ", "\\(\\beta = \\)", " ", input$beta_beta, "\\()\\)", " and ", case_when(input$lower_tail_beta == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_beta, "\\()\\)", " ", "\\( = \\)", " ", round(pbeta(input$x1_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE), 4)),
                                                                                                                                         input$lower_tail_beta == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_beta, "\\()\\)", " ", "\\( = \\)", " ", round(pbeta(input$x2_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), 4)),
                                                                                                                                         input$lower_tail_beta == "interval" ~ paste0("\\(P(\\)", input$a_beta, " ", "\\(\\leq X\\leq \\)", " ", input$b_beta, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_beta > input$b_beta, "a must be less than or equal to b", round(pbeta(input$b_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE) - pbeta(input$a_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Binomial") {
      withMathJax(
        paste0("\\(X \\sim Bin(n = \\)", " ", input$n_binomial, ", ", "\\(p = \\)", " ", input$p_binomial, "\\()\\)", " and ", case_when(input$lower_tail_binomial == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_binomial, "\\()\\)", " ", "\\( = \\)", " ", round(pbinom(input$x1_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), 4)),
                                                                                                input$lower_tail_binomial == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_binomial, "\\()\\)", " ", "\\( = \\)", " ", round(pbinom(input$x2_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE), 4)),
                                                                                                input$lower_tail_binomial == "interval" ~ paste0("\\(P(\\)", input$a_binomial, " ", "\\(\\leq X\\leq \\)", " ", input$b_binomial, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_binomial > input$b_binomial, "a must be less than or equal to b", round(pbinom(input$b_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE) - pbinom(input$a_binomial - 1, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Cauchy") {
      withMathJax(
        paste0("\\(X \\sim Cauchy(x_0 = \\)", " ", input$location_cauchy, ", ", "\\(\\gamma = \\)", " ", input$scale_cauchy, "\\()\\)", " and ", case_when(input$lower_tail_cauchy == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_cauchy, "\\()\\)", " ", "\\( = \\)", " ", round(pcauchy(input$x1_cauchy, location = input$location_cauchy, scale = input$scale_cauchy, lower.tail = TRUE), 4)),
                                                                                                                                         input$lower_tail_cauchy == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_cauchy, "\\()\\)", " ", "\\( = \\)", " ", round(pcauchy(input$x2_cauchy, location = input$location_cauchy, scale = input$scale_cauchy, lower.tail = FALSE), 4)),
                                                                                                                                         input$lower_tail_cauchy == "interval" ~ paste0("\\(P(\\)", input$a_cauchy, " ", "\\(\\leq X\\leq \\)", " ", input$b_cauchy, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_cauchy > input$b_cauchy, "a must be less than or equal to b", round(pcauchy(input$b_cauchy, location = input$location_cauchy, scale = input$scale_cauchy, lower.tail = TRUE) - pcauchy(input$a_cauchy, location = input$location_cauchy, scale = input$scale_cauchy, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Chi-square") {
      withMathJax(
        paste0("\\(X \\sim \\chi^2(df = \\)", " ", input$df_chisquare, "\\()\\)", " and ", case_when(input$lower_tail_chisquare == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_chisquare, "\\()\\)", " ", "\\( = \\)", " ", round(pchisq(input$x1_chisquare, df = input$df_chisquare, lower.tail = TRUE), 4)),
                                                                       input$lower_tail_chisquare == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_chisquare, "\\()\\)", " ", "\\( = \\)", " ", round(pchisq(input$x2_chisquare, df = input$df_chisquare, lower.tail = FALSE), 4)),
                                                                       input$lower_tail_chisquare == "interval" ~ paste0("\\(P(\\)", input$a_chisquare, " ", "\\(\\leq X\\leq \\)", " ", input$b_chisquare, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_chisquare > input$b_chisquare, "a must be less than or equal to b", round(pchisq(input$b_chisquare, df = input$df_chisquare, lower.tail = TRUE) - pchisq(input$a_chisquare, df = input$df_chisquare, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Exponential") {
      withMathJax(
        paste0("\\(X \\sim \\exp(\\lambda = \\)", " ", input$rate_exponential, "\\()\\)", " and ", case_when(input$lower_tail_exponential == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_exponential, "\\()\\)", " ", "\\( = \\)", " ", round(pexp(input$x1_exponential, rate = input$rate_exponential, lower.tail = TRUE), 4)),
                                                                                                     input$lower_tail_exponential == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_exponential, "\\()\\)", " ", "\\( = \\)", " ", round(pexp(input$x2_exponential, rate = input$rate_exponential, lower.tail = FALSE), 4)),
                                                                                                     input$lower_tail_exponential == "interval" ~ paste0("\\(P(\\)", input$a_exponential, " ", "\\(\\leq X\\leq \\)", " ", input$b_exponential, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_exponential > input$b_exponential, "a must be less than or equal to b", round(pexp(input$b_exponential, rate = input$rate_exponential, lower.tail = TRUE) - pexp(input$a_exponential, rate = input$rate_exponential, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Fisher") {
      withMathJax(
        paste0("\\(X \\sim F(df_1 = \\)", " ", input$df1_fisher, ", ", "\\(df_2\\)", " = ", input$df2_fisher, "\\()\\)", " and ", case_when(input$lower_tail_fisher == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_fisher, "\\()\\)", " ", "\\( = \\)", " ", round(pf(input$x1_fisher, df1 = input$df1_fisher, df2 = input$df2_fisher, lower.tail = TRUE), 4)),
                                                                                                    input$lower_tail_fisher == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_fisher, "\\()\\)", " ", "\\( = \\)", " ", round(pf(input$x2_fisher, df1 = input$df1_fisher, df2 = input$df2_fisher, lower.tail = FALSE), 4)),
                                                                                                    input$lower_tail_fisher == "interval" ~ paste0("\\(P(\\)", input$a_fisher, " ", "\\(\\leq X\\leq \\)", " ", input$b_fisher, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_fisher > input$b_fisher, "a must be less than or equal to b", round(pf(input$b_fisher, df1 = input$df1_fisher, df = input$df2_fisher, lower.tail = TRUE) - pf(input$a_fisher, df1 = input$df1_fisher, df = input$df2_fisher, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Gamma") {
      withMathJax(
        paste0("\\(X \\sim Gamma(\\alpha = \\)", " ", input$alpha_gamma, ", ", "\\(\\beta = \\)", " ", input$beta_gamma, "\\()\\)", " and ", case_when(input$lower_tail_gamma == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_gamma, "\\()\\)", " ", "\\( = \\)", " ", round(pgamma(input$x1_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE), 4)),
                                                                                                                                                    input$lower_tail_gamma == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_gamma, "\\()\\)", " ", "\\( = \\)", " ", round(pgamma(input$x2_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), 4)),
                                                                                                                                                    input$lower_tail_gamma == "interval" ~ paste0("\\(P(\\)", input$a_gamma, " ", "\\(\\leq X\\leq \\)", " ", input$b_gamma, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_gamma > input$b_gamma, "a must be less than or equal to b", round(pgamma(input$b_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE) - pgamma(input$a_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Geometric (I)") {
      withMathJax(
        paste0("\\(X \\sim Geom(p = \\)", " ", input$p_geometric, "\\()\\)", " and ", case_when(input$lower_tail_geometric == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_geometric, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x1_geometric, prob = input$p_geometric, lower.tail = TRUE), 4)),
                                                                                                          input$lower_tail_geometric == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_geometric, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x2_geometric, prob = input$p_geometric, lower.tail = FALSE), 4)),
                                                                                                          input$lower_tail_geometric == "interval" ~ paste0("\\(P(\\)", input$a_geometric, " ", "\\(\\leq X\\leq \\)", " ", input$b_geometric, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_geometric > input$b_geometric, "a must be less than or equal to b", round(pgeom(input$b_geometric, prob = input$p_geometric, lower.tail = TRUE) - pgeom(input$a_geometric - 1, prob = input$p_geometric, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Geometric (II)") {
      withMathJax(
        paste0("\\(X \\sim Geom(p = \\)", " ", input$p_geometric2, "\\()\\)", " and ", case_when(input$lower_tail_geometric2 == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_geometric2, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x1_geometric2-1, prob = input$p_geometric2, lower.tail = TRUE), 4)),
                                                                                                input$lower_tail_geometric2 == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_geometric2, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x2_geometric2-1, prob = input$p_geometric2, lower.tail = FALSE), 4)),
                                                                                                input$lower_tail_geometric2 == "interval" ~ paste0("\\(P(\\)", input$a_geometric2, " ", "\\(\\leq X\\leq \\)", " ", input$b_geometric2, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_geometric2 > input$b_geometric2, "a must be less than or equal to b", round(pgeom(input$b_geometric2-1, prob = input$p_geometric2, lower.tail = TRUE) - pgeom(input$a_geometric2 - 2, prob = input$p_geometric2, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Hypergeometric") {
      withMathJax(
        paste0("\\(X \\sim HG(n = \\)", " ", input$n_hypergeometric, ", ", "\\(N = \\)", " ", input$N_hypergeometric, ", ", "\\(M = \\)", " ", input$M_hypergeometric, "\\()\\)", " and ", case_when(input$lower_tail_hypergeometric == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", round(phyper(input$x1_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), 4)),
                                                                                                input$lower_tail_hypergeometric == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", round(phyper(input$x2_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE), 4)),
                                                                                                input$lower_tail_hypergeometric == "interval" ~ paste0("\\(P(\\)", input$a_hypergeometric, " ", "\\(\\leq X\\leq \\)", " ", input$b_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_hypergeometric > input$b_hypergeometric, "a must be less than or equal to b", round(phyper(input$b_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE) - phyper(input$a_hypergeometric - 1, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Logistic") {
      withMathJax(
        paste0("\\(X \\sim Logi(\\mu = \\)", " ", input$location_logistic, ", ", "\\(s = \\)", " ", input$scale_logistic, "\\()\\)", " and ", case_when(input$lower_tail_logistic == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_logistic, "\\()\\)", " ", "\\( = \\)", " ", round(plogis(input$x1_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE), 4)),
                                                                                                                                                           input$lower_tail_logistic == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_logistic, "\\()\\)", " ", "\\( = \\)", " ", round(plogis(input$x2_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), 4)),
                                                                                                                                                           input$lower_tail_logistic == "interval" ~ paste0("\\(P(\\)", input$a_logistic, " ", "\\(\\leq X\\leq \\)", " ", input$b_logistic, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_logistic > input$b_logistic, "a must be less than or equal to b", round(plogis(input$b_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE) - plogis(input$a_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Log-Normal") {
      withMathJax(
        paste0("\\(X \\sim Lognormal(\\mu = \\)", " ", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", paste0("\\(\\sigma^2 = \\)", " ", input$variance_lognormal), paste0("\\(\\sigma^2 = \\)", " ", input$sd_lognormal^2)), "\\()\\)", " and ", case_when(input$lower_tail_lognormal == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_lognormal, "\\()\\)", " ", "\\( = \\)", " " , round(plnorm(input$x1_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE), 4)),
                                                                                                                                                                                                                                                                               input$lower_tail_lognormal == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_lognormal, "\\()\\)", " ", "\\( = \\)", " ", "\\( = \\)", " ", round(plnorm(input$x2_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = FALSE), 4)),
                                                                                                                                                                                                                                                                               input$lower_tail_lognormal == "interval" ~ paste0("\\(P(\\)", input$a_lognormal, " ", "\\(\\leq X\\leq \\)", " ", input$b_lognormal, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_lognormal > input$b_lognormal, "a must be less than or equal to b", round(plnorm(input$b_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE) - plnorm(input$a_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Negative Binomial (I)") {
      withMathJax(
        paste0("\\(X \\sim NG(r = \\)", " ", input$r_negativebinomial, ", ", "\\(p = \\)", " ", input$p_negativebinomial, "\\()\\)", " and ", case_when(input$lower_tail_negativebinomial == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$x1_negativebinomial, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), 4)),
                                                                                                                                         input$lower_tail_negativebinomial == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$x2_negativebinomial, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE), 4)),
                                                                                                                                         input$lower_tail_negativebinomial == "interval" ~ paste0("\\(P(\\)", input$a_negativebinomial, " ", "\\(\\leq X\\leq \\)", " ", input$b_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_negativebinomial > input$b_negativebinomial, "a must be less than or equal to b", round(pnbinom(input$b_negativebinomial, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE) - pnbinom(input$a_negativebinomial - 1, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Negative Binomial (II)") {
      withMathJax(
        paste0("\\(X \\sim NG(r = \\)", " ", input$r_negativebinomial2, ", ", "\\(p = \\)", " ", input$p_negativebinomial2, "\\()\\)", " and ", case_when(input$lower_tail_negativebinomial2 == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_negativebinomial2, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$x1_negativebinomial2-input$r_negativebinomial2, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE), 4)),
                                                                                                                                                        input$lower_tail_negativebinomial2 == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_negativebinomial2, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$x2_negativebinomial2-input$r_negativebinomial2, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = FALSE), 4)),
                                                                                                                                                        input$lower_tail_negativebinomial2 == "interval" ~ paste0("\\(P(\\)", input$a_negativebinomial2, " ", "\\(\\leq X\\leq \\)", " ", input$b_negativebinomial2, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_negativebinomial2 > input$b_negativebinomial2, "a must be less than or equal to b", round(pnbinom(input$b_negativebinomial2-input$r_negativebinomial2, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE) - pnbinom(input$a_negativebinomial2 - 1 - input$r_negativebinomial2, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Normal") {
      withMathJax(
        paste0("\\(X \\sim \\mathcal{N}(\\mu = \\)", " ", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", paste0("\\(\\sigma^2 = \\)", " ", input$variance_normal), paste0("\\(\\sigma^2 = \\)", " ", input$sd_normal^2)), "\\()\\)", " and ", case_when(input$lower_tail_normal == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_normal, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z \\leq \\)", " ", round((input$x1_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), "\\()\\)", " ", "\\( = \\)", " " , round(pnorm(input$x1_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE), 4)),
                                                                                                                                                                                                    input$lower_tail_normal == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_normal, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z > \\)", " ", round((input$x2_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(input$x2_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), 4)),
                                                                                                                                                                                                    input$lower_tail_normal == "interval" ~ paste0("\\(P(\\)", input$a_normal, " ", "\\(\\leq X\\leq \\)", " ", input$b_normal, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(\\)", round((input$a_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), " ", "\\(\\leq Z\\leq \\)", " ", round((input$b_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_normal > input$b_normal, "a must be less than or equal to b", round(pnorm(input$b_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE) - pnorm(input$a_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Poisson") {
      withMathJax(
        paste0("\\(X \\sim Pois(\\lambda = \\)", " ", input$lambda_poisson, "\\()\\)", " and ", case_when(input$lower_tail_poisson == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_poisson, "\\()\\)", " ", "\\( = \\)", " ", round(ppois(input$x1_poisson, lambda = input$lambda_poisson, lower.tail = TRUE), 4)),
                                                                           input$lower_tail_poisson == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_poisson, "\\()\\)", " ", "\\( = \\)", " ", round(ppois(input$x2_poisson, lambda = input$lambda_poisson, lower.tail = FALSE), 4)),
                                                                           input$lower_tail_poisson == "interval" ~ paste0("\\(P(\\)", input$a_poisson, " ", "\\(\\leq X\\leq \\)", " ", input$b_poisson, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_poisson > input$b_poisson, "a must be less than or equal to b", round(ppois(input$b_poisson, lambda = input$lambda_poisson, lower.tail = TRUE) - ppois(input$a_poisson - 1, lambda = input$lambda_poisson, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Student") {
      withMathJax(
        paste0("\\(X \\sim St(df = \\)", " ", input$df_student, "\\()\\)", " and ", case_when(input$lower_tail_student == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_student, "\\()\\)", " ", "\\( = \\)", " ", round(pt(input$x1_student, df = input$df_student, lower.tail = TRUE), 4)),
                                                                      input$lower_tail_student == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_student, "\\()\\)", " ", "\\( = \\)", " ", round(pt(input$x2_student, df = input$df_student, lower.tail = FALSE), 4)),
                                                                      input$lower_tail_student == "interval" ~ paste0("\\(P(\\)", input$a_student, " ", "\\(\\leq X\\leq \\)", " ", input$b_student, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_student > input$b_student, "a must be less than or equal to b", round(pt(input$b_student, df = input$df_student, lower.tail = TRUE) - pt(input$a_student, df = input$df_student, lower.tail = TRUE), 4)))))
      )
    } else if (input$distribution == "Weibull") {
      withMathJax(
        paste0("\\(X \\sim Weibull(\\alpha = \\)", " ", input$alpha_weibull, ", ", "\\(\\beta = \\)", " ", input$beta_weibull, "\\()\\)", " and ", case_when(input$lower_tail_weibull == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_weibull, "\\()\\)", " ", "\\( = \\)", " ", round(pweibull(input$x1_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE), 4)),
                                                                                                                                                       input$lower_tail_weibull == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_weibull, "\\()\\)", " ", "\\( = \\)", " ", round(pweibull(input$x2_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), 4)),
                                                                                                                                                       input$lower_tail_weibull == "interval" ~ paste0("\\(P(\\)", input$a_weibull, " ", "\\(\\leq X\\leq \\)", " ", input$b_weibull, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_weibull > input$b_weibull, "a must be less than or equal to b", round(pweibull(input$b_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE) - pweibull(input$a_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE), 4)))))
      )
    } else {
      print("in progress")
    }
  })
  
  output$betaPlot_lower <- renderPlot({
    funcShaded <- function(x) {
      y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
      y[x > input$x1_beta] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$betaPlot_upper <- renderPlot({
    funcShaded <- function(x) {
      y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
      y[x < input$x2_beta] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$betaPlot_interval <- renderPlot({
    funcShaded <- function(x) {
      y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
      y[x < input$a_beta | x > input$b_beta] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$binomialPlot_lower <- renderPlot({
    p <- data.frame(heads = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), prob = dbinom(x = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), size = input$n_binomial, prob = input$p_binomial)) %>%
      mutate(Heads = ifelse(heads <= input$x1_binomial, "2", "Other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$binomialPlot_upper <- renderPlot({
    p <- data.frame(heads = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), prob = dbinom(x = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), size = input$n_binomial, prob = input$p_binomial)) %>%
      mutate(Heads = ifelse(heads > input$x2_binomial, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$binomialPlot_interval <- renderPlot({
    p <- data.frame(heads = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), prob = dbinom(x = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), size = input$n_binomial, prob = input$p_binomial)) %>%
      mutate(Heads = ifelse(heads >= input$a_binomial & heads <= input$b_binomial, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$cauchyPlot_lower <- renderPlot({
    funcShaded <- function(x) {
      y <- dcauchy(x, location = input$location_cauchy, scale = input$scale_cauchy)
      y[x > input$x1_cauchy] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(input$location_cauchy - (6*input$scale_cauchy), input$location_cauchy + (6*input$scale_cauchy))), aes(x = x)) +
      stat_function(fun = dcauchy, args = list(location = input$location_cauchy, scale = input$scale_cauchy)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$cauchyPlot_upper <- renderPlot({
    funcShaded <- function(x) {
      y <- dcauchy(x, location = input$location_cauchy, scale = input$scale_cauchy)
      y[x < input$x2_cauchy] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(input$location_cauchy - (6*input$scale_cauchy), input$location_cauchy + (6*input$scale_cauchy))), aes(x = x)) +
      stat_function(fun = dcauchy, args = list(location = input$location_cauchy, scale = input$scale_cauchy)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$cauchyPlot_interval <- renderPlot({
    funcShaded <- function(x) {
      y <- dcauchy(x, location = input$location_cauchy, scale = input$scale_cauchy)
      y[x < input$a_cauchy | x > input$b_cauchy] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(input$location_cauchy - (6*input$scale_cauchy), input$location_cauchy + (6*input$scale_cauchy))), aes(x = x)) +
      stat_function(fun = dcauchy, args = list(location = input$location_cauchy, scale = input$scale_cauchy)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$chisquarePlot_lower <- renderPlot({
    funcShaded <- function(x) {
      y <- dchisq(x, df = input$df_chisquare)
      y[x > input$x1_chisquare] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qchisq(0.99999, df = input$df_chisquare, lower.tail = FALSE), qchisq(0.99999, df = input$df_chisquare, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$chisquarePlot_upper <- renderPlot({
    funcShaded <- function(x) {
      y <- dchisq(x, df = input$df_chisquare)
      y[x < input$x2_chisquare] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qchisq(0.99999, df = input$df_chisquare, lower.tail = FALSE), qchisq(0.99999, df = input$df_chisquare, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$chisquarePlot_interval <- renderPlot({
    funcShaded <- function(x) {
      y <- dchisq(x, df = input$df_chisquare)
      y[x < input$a_chisquare | x > input$b_chisquare] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qchisq(0.99999, df = input$df_chisquare, lower.tail = FALSE), qchisq(0.99999, df = input$df_chisquare, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$exponentialPlot_lower <- renderPlot({
    funcShaded <- function(x) {
      y <- dexp(x, rate = input$rate_exponential)
      y[x > input$x1_exponential] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qexp(0.99999, rate = input$rate_exponential, lower.tail = FALSE), qexp(0.99999, rate = input$rate_exponential, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$exponentialPlot_upper <- renderPlot({
    funcShaded <- function(x) {
      y <- dexp(x, rate = input$rate_exponential)
      y[x < input$x2_exponential] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qexp(0.99999, rate = input$rate_exponential, lower.tail = FALSE), qexp(0.99999, rate = input$rate_exponential, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$exponentialPlot_interval <- renderPlot({
    funcShaded <- function(x) {
      y <- dexp(x, rate = input$rate_exponential)
      y[x < input$a_exponential | x > input$b_exponential] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qexp(0.99999, rate = input$rate_exponential, lower.tail = FALSE), qexp(0.99999, rate = input$rate_exponential, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$fisherPlot_lower <- renderPlot({
    funcShaded <- function(x) {
      y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
      y[x > input$x1_fisher] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
      stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$fisherPlot_upper <- renderPlot({
    funcShaded <- function(x) {
      y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
      y[x < input$x2_fisher] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
      stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$fisherPlot_interval <- renderPlot({
    funcShaded <- function(x) {
      y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
      y[x < input$a_fisher | x > input$b_fisher] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
      stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$gammaPlot_lower <- renderPlot({
    funcShaded <- function(x) {
      y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
      y[x > input$x1_gamma] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$gammaPlot_upper <- renderPlot({
    funcShaded <- function(x) {
      y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
      y[x < input$x2_gamma] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$gammaPlot_interval <- renderPlot({
    funcShaded <- function(x) {
      y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
      y[x < input$a_gamma | x > input$b_gamma] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$geometricPlot_lower <- renderPlot({
    p <- data.frame(heads = 0:(input$p_geometric + (5*sqrt((1-input$p_geometric)/(input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5*sqrt((1-input$p_geometric)/(input$p_geometric^2)))), prob = input$p_geometric)) %>%
      mutate(Heads = ifelse(heads <= input$x1_geometric, "2", "Other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$geometricPlot_upper <- renderPlot({
    p <- data.frame(heads = 0:(input$p_geometric + (5*sqrt((1-input$p_geometric)/(input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5*sqrt((1-input$p_geometric)/(input$p_geometric^2)))), prob = input$p_geometric)) %>%
      mutate(Heads = ifelse(heads > input$x2_geometric, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$geometricPlot_interval <- renderPlot({
    p <- data.frame(heads = 0:(input$p_geometric + (5*sqrt((1-input$p_geometric)/(input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5*sqrt((1-input$p_geometric)/(input$p_geometric^2)))), prob = input$p_geometric)) %>%
      mutate(Heads = ifelse(heads >= input$a_geometric & heads <= input$b_geometric, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$geometric2Plot_lower <- renderPlot({
    p <- data.frame(heads = 1:(input$p_geometric2 + (5*sqrt((1-input$p_geometric2)/(input$p_geometric2^2)))+1), prob = dgeom(x = 0:(input$p_geometric2 + (5*sqrt((1-input$p_geometric2)/(input$p_geometric2^2)))), prob = input$p_geometric2)) %>%
      mutate(Heads = ifelse(heads <= input$x1_geometric2, "2", "Other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$geometric2Plot_upper <- renderPlot({
    p <- data.frame(heads = 1:(input$p_geometric2 + (5*sqrt((1-input$p_geometric2)/(input$p_geometric2^2)))+1), prob = dgeom(x = 0:(input$p_geometric2 + (5*sqrt((1-input$p_geometric2)/(input$p_geometric2^2)))), prob = input$p_geometric2)) %>%
      mutate(Heads = ifelse(heads > input$x2_geometric2, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$geometric2Plot_interval <- renderPlot({
    p <- data.frame(heads = 1:(input$p_geometric2 + (5*sqrt((1-input$p_geometric2)/(input$p_geometric2^2)))+1), prob = dgeom(x = 0:(input$p_geometric2 + (5*sqrt((1-input$p_geometric2)/(input$p_geometric2^2)))), prob = input$p_geometric2)) %>%
      mutate(Heads = ifelse(heads >= input$a_geometric2 & heads <= input$b_geometric2, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$hypergeometricPlot_lower <- renderPlot({
    p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
      mutate(Heads = ifelse(heads <= input$x1_hypergeometric, "2", "Other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$hypergeometricPlot_upper <- renderPlot({
    p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
      mutate(Heads = ifelse(heads > input$x2_hypergeometric, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$hypergeometricPlot_interval <- renderPlot({
    p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
      mutate(Heads = ifelse(heads >= input$a_hypergeometric & heads <= input$b_hypergeometric, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$logisticPlot_lower <- renderPlot({
    funcShaded <- function(x) {
      y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
      y[x > input$x1_logistic] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$logisticPlot_upper <- renderPlot({
    funcShaded <- function(x) {
      y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
      y[x < input$x2_logistic] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$logisticPlot_interval <- renderPlot({
    funcShaded <- function(x) {
      y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
      y[x < input$a_logistic | x > input$b_logistic] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$lognormalPlot_lower <- renderPlot({
    funcShaded <- function(x) {
      y <- dlnorm(x, meanlog=input$mean_lognormal,
                 sdlog=ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal))
      y[x > input$x1_lognormal] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(0,qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
      stat_function(fun = dlnorm, args = list(meanlog=input$mean_lognormal,
                                             sdlog=ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal))) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$lognormalPlot_upper <- renderPlot({
    funcShaded <- function(x) {
      y <- dlnorm(x, meanlog=input$mean_lognormal,
                 sdlog=ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal))
      y[x < input$x2_lognormal] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(0,qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
      stat_function(fun = dlnorm, args = list(meanlog=input$mean_lognormal,
                                             sdlog=ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal))) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$lognormalPlot_interval <- renderPlot({
    funcShaded <- function(x) {
      y <- dlnorm(x, meanlog=input$mean_lognormal,
                 sdlog=ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal))
      y[x < input$a_lognormal | x > input$b_lognormal] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(0,qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
      stat_function(fun = dlnorm, args = list(meanlog=input$mean_lognormal,
                                             sdlog=ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal))) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$negativebinomialPlot_lower <- renderPlot({
    p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
      mutate(Heads = ifelse(heads <= input$x1_negativebinomial, "2", "Other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$negativebinomialPlot_upper <- renderPlot({
    p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
      mutate(Heads = ifelse(heads > input$x2_negativebinomial, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$negativebinomialPlot_interval <- renderPlot({
    p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
      mutate(Heads = ifelse(heads >= input$a_negativebinomial & heads <= input$b_negativebinomial, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$negativebinomial2Plot_lower <- renderPlot({
    p <- data.frame(heads = input$r_negativebinomial2:(qnbinom(0.999, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE) + input$r_negativebinomial2), prob = dnbinom(x = 0:qnbinom(0.999, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE), size = input$r_negativebinomial2, prob = input$p_negativebinomial2)) %>%
      mutate(Heads = ifelse(heads <= input$x1_negativebinomial2, "2", "Other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$negativebinomial2Plot_upper <- renderPlot({
    p <- data.frame(heads = input$r_negativebinomial2:(qnbinom(0.999, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE) + input$r_negativebinomial2), prob = dnbinom(x = 0:qnbinom(0.999, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE), size = input$r_negativebinomial2, prob = input$p_negativebinomial2)) %>%
      mutate(Heads = ifelse(heads > input$x2_negativebinomial2, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$negativebinomial2Plot_interval <- renderPlot({
    p <- data.frame(heads = input$r_negativebinomial2:(qnbinom(0.999, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE) + input$r_negativebinomial2), prob = dnbinom(x = 0:qnbinom(0.999, size = input$r_negativebinomial2, prob = input$p_negativebinomial2, lower.tail = TRUE), size = input$r_negativebinomial2, prob = input$p_negativebinomial2)) %>%
      mutate(Heads = ifelse(heads >= input$a_negativebinomial2 & heads <= input$b_negativebinomial2, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$normalPlot_lower <- renderPlot({
    funcShaded <- function(x) {
      y <- dnorm(x, mean=input$mean_normal,
                 sd=ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal))
      y[x > input$x1_normal] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qnorm(0.99999, mean=input$mean_normal, sd=ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean=input$mean_normal, sd=ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dnorm, args = list(mean=input$mean_normal,
                                             sd=ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal))) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$normalPlot_upper <- renderPlot({
    funcShaded <- function(x) {
      y <- dnorm(x, mean=input$mean_normal,
              sd=ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal))
      y[x < input$x2_normal] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qnorm(0.99999, mean=input$mean_normal, sd=ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean=input$mean_normal, sd=ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dnorm, args = list(mean=input$mean_normal,
                                          sd=ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal))) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$normalPlot_interval <- renderPlot({
    funcShaded <- function(x) {
      y <- dnorm(x, mean=input$mean_normal,
              sd=ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal))
      y[x < input$a_normal | x > input$b_normal] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qnorm(0.99999, mean=input$mean_normal, sd=ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean=input$mean_normal, sd=ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dnorm, args = list(mean=input$mean_normal,
                                             sd=ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal))) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$poissonPlot_lower <- renderPlot({
    p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
      mutate(Heads = ifelse(heads <= input$x1_poisson, "2", "Other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$poissonPlot_upper <- renderPlot({
    p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
      mutate(Heads = ifelse(heads > input$x2_poisson, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$poissonPlot_interval <- renderPlot({
    p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
      mutate(Heads = ifelse(heads >= input$a_poisson & heads <= input$b_poisson, "2", "other")) %>%
      ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
      geom_col() +
      geom_text(
        aes(label = round(prob,3), y = prob + 0.005),
        position = position_dodge(0.9),
        size = 3,
        vjust = 0
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$studentPlot_lower <- renderPlot({
    funcShaded <- function(x) {
      y <- dt(x, df = input$df_student)
      y[x > input$x1_student] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dt, args = list(df = input$df_student)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$studentPlot_upper <- renderPlot({
    funcShaded <- function(x) {
      y <- dt(x, df = input$df_student)
      y[x < input$x2_student] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dt, args = list(df = input$df_student)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$studentPlot_interval <- renderPlot({
    funcShaded <- function(x) {
      y <- dt(x, df = input$df_student)
      y[x < input$a_student | x > input$b_student] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dt, args = list(df = input$df_student)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$weibullPlot_lower <- renderPlot({
    funcShaded <- function(x) {
      y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
      y[x > input$x1_weibull] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$weibullPlot_upper <- renderPlot({
    funcShaded <- function(x) {
      y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
      y[x < input$x2_weibull] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  output$weibullPlot_interval <- renderPlot({
    funcShaded <- function(x) {
      y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
      y[x < input$a_weibull | x > input$b_weibull] <- NA
      return(y)
    }
    p <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
      stat_function(fun=funcShaded, geom="area", alpha=0.8) +
      theme_minimal() +
      ggtitle(paste0(input$distribution, " distribution")) +
      theme(plot.title = element_text(face="bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  })
  
  output$parameters_distribution <- renderUI({
    if (input$distribution == "Beta") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\frac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)} x^{\\alpha-1} (1 - x)^{\\beta - 1} $$"),
        helpText("where \\( 0 \\leq x \\leq 1, \\alpha > 0, \\beta > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\frac{\\alpha}{\\alpha + \\beta} = \\)", round(input$alpha_beta  / (input$alpha_beta + input$beta_beta), 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\frac{\\alpha\\beta}{(\\alpha + \\beta)^2(\\alpha + \\beta+1)}} = \\)", round(sqrt((input$alpha_beta * input$beta_beta) / (((input$alpha_beta + input$beta_beta)^2)*(input$alpha_beta + input$beta_beta + 1))), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\frac{\\alpha\\beta}{(\\alpha + \\beta)^2(\\alpha + \\beta+1)} = \\)", round((input$alpha_beta * input$beta_beta) / (((input$alpha_beta + input$beta_beta)^2)*(input$alpha_beta + input$beta_beta + 1)), 3)))
    } else if (input$distribution == "Binomial") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = P(X = x) = \\binom{n}{x}p^x(1-p)^{n-x}$$ "),
        helpText("where \\( x = 0, 1, \\dots, n\\) and \\( 0 \\leq p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = np = \\)", round(input$n_binomial * input$p_binomial, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{np(1-p)} = \\)", round(sqrt(input$n_binomial * input$p_binomial * (1 - input$p_binomial)), 3)),
        helpText("\\(\\sigma^2 = Var(X) = np(1-p) = \\)", round(input$n_binomial * input$p_binomial * (1 - input$p_binomial), 3)))
    } else if (input$distribution == "Cauchy") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\frac{1}{\\pi\\gamma \\Big[ 1 + \\big(\\frac{x - x_0}{\\gamma}\\big)^2\\Big]} $$"),
        helpText("where \\( -\\infty < x_0 < \\infty, -\\infty < x < \\infty, y > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = Undefined\\)"),
        helpText("\\(\\sigma = SD(X) = Undefined\\)"),
        helpText("\\(\\sigma^2 = Var(X) = Undefined \\)"),
        helpText("\\(median = x_0 = \\)", round(input$location_cauchy, 3)),
        helpText("\\(mode = x_0 = \\)", round(input$location_cauchy, 3)))
    } else if (input$distribution == "Chi-square") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\frac{1}{2^{df/2}\\Gamma\\big(\\frac{df}{2}\\big)} x^{df/2 - 1} e^{-x/2} $$"),
        helpText("where \\( x > 0, df > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = df = \\)", round(input$df_chisquare, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{2df} = \\)", round(sqrt(2*input$df_chisquare), 3)),
        helpText("\\(\\sigma^2 = Var(X) = 2df = \\)", round(2*input$df_chisquare, 3)))
    } else if (input$distribution == "Exponential") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\lambda e^{-\\lambda x} $$"),
        helpText("where \\( x > 0, \\lambda > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\frac{1}{\\lambda} = \\)", round(1/input$rate_exponential, 3)),
        helpText("\\(\\sigma = SD(X) = \\frac{1}{\\lambda} = \\)", round(1/input$rate_exponential, 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\frac{1}{\\lambda^2} = \\)", round(1/(input$rate_exponential^2), 3)))
    } else if (input$distribution == "Fisher") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\frac{\\Gamma\\big(\\frac{df_1 + df_2}{2}\\big) \\big(\\frac{df_1}{df_2}\\big)^{\\frac{df_1}{2}}x^{\\frac{df_1}{2}-1}}{\\Gamma\\big(\\frac{df_1}{2}\\big)\\Gamma\\big(\\frac{df_2}{2}\\big)\\big(1 + \\frac{df_1 x}{df_2}\\big)^{\\frac{df_1 + df_2}{2}}} $$"),
        helpText("where \\( df_1, df_2 > 0, x \\geq 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\frac{df_2}{df_2 - 2} = \\)", ifelse(input$df2_fisher > 2, round(input$df2_fisher / (input$df2_fisher - 2), 3), "Undefined")),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\frac{2df^2_2(df_1 + df_2 - 2)}{df_1(df_2 - 2)^2(df_2 - 4)}} = \\)", ifelse(input$df2_fisher > 4, round(sqrt((2*input$df2_fisher^2 * (input$df1_fisher + input$df2_fisher - 2)) / (input$df1_fisher * (input$df2_fisher - 2)^2*(input$df2_fisher - 4))), 3), "Undefined")),
        helpText("\\(\\sigma^2 = Var(X) = \\frac{2df^2_2(df_1 + df_2 - 2)}{df_1(df_2 - 2)^2(df_2 - 4)} = \\)", ifelse(input$df2_fisher > 4, round((2*input$df2_fisher^2 * (input$df1_fisher + input$df2_fisher - 2)) / (input$df1_fisher * (input$df2_fisher - 2)^2*(input$df2_fisher - 4)), 3), "Undefined")))
    } else if (input$distribution == "Gamma") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\frac{\\beta^\\alpha}{\\Gamma(\\alpha)} x^{\\alpha -1}e^{-\\beta x} $$"),
        helpText("where \\( x > 0, \\alpha > 0, \\beta > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\frac{\\alpha}{\\beta} = \\)", round(input$alpha_gamma  / input$beta_gamma, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\frac{\\alpha}{\\beta^2}} = \\)", round(sqrt(input$alpha_gamma / (input$beta_gamma^2)), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\frac{\\alpha}{\\beta^2} = \\)", round(input$alpha_gamma / (input$beta_gamma^2), 3)))
    } else if (input$distribution == "Geometric (I)") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = P(X = x) = (1 - p)^x p $$"),
        helpText("where \\( x = 0, 1, 2, \\dots \\) and \\( 0 < p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\frac{1-p}{p} = \\)", round((1 - input$p_geometric) / input$p_geometric, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\frac{1-p}{p^2}} = \\)", round(sqrt((1 - input$p_geometric) / (input$p_geometric^2)), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\frac{1-p}{p^2} = \\)", round((1 - input$p_geometric) / (input$p_geometric^2), 3)))
    } else if (input$distribution == "Geometric (II)") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = P(X = x) = (1 - p)^{x-1} p $$"),
        helpText("where \\( x = 1, 2, \\dots \\) and \\( 0 < p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\frac{1}{p} = \\)", round((1) / input$p_geometric2, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\frac{1-p}{p^2}} = \\)", round(sqrt((1 - input$p_geometric2) / (input$p_geometric2^2)), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\frac{1-p}{p^2} = \\)", round((1 - input$p_geometric2) / (input$p_geometric2^2), 3)))
    } else if (input$distribution == "Hypergeometric") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = P(X = x) = \\frac{\\binom{M}{x} \\binom{N-M}{n-x}}{\\binom{N}{n}}  $$"),
        helpText("for \\( x = 0, 1, \\dots , n\\)"),
        helpText("where \\( x \\leq M \\) and \\( n - x \\leq N - M \\)"),
        br(),
        helpText("\\(\\mu = E(X) = n\\frac{M}{N} = \\)", round(input$n_hypergeometric*(input$M_hypergeometric/input$N_hypergeometric), 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{n\\frac{M}{N}\\big(1 - \\frac{M}{N}\\big)\\big(\\frac{N-n}{N-1}\\big)} = \\)", round(sqrt(input$n_hypergeometric*input$M_hypergeometric/input$N_hypergeometric*(1-(input$M_hypergeometric/input$N_hypergeometric))*((input$N_hypergeometric-input$n_hypergeometric)/(input$N_hypergeometric-1))), 3)),
        helpText("\\(\\sigma^2 = Var(X) = n\\frac{M}{N}\\big(1 - \\frac{M}{N}\\big)\\big(\\frac{N-n}{N-1}\\big) = \\)", round(input$n_hypergeometric*input$M_hypergeometric/input$N_hypergeometric*(1-(input$M_hypergeometric/input$N_hypergeometric))*((input$N_hypergeometric-input$n_hypergeometric)/(input$N_hypergeometric-1)), 3)))
    } else if (input$distribution == "Logistic") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\frac{e^{-\\frac{x-\\mu}{s}}}{s\\big(1 + e^{-\\frac{x - \\mu}{s}}\\big)^2} $$"),
        helpText("where \\( -\\infty < x < \\infty, -\\infty < \\mu < \\infty, s > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\mu = \\)", round(input$location_logistic, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\frac{s^2\\pi^2}{3}} = \\)", round(sqrt(((input$scale_logistic^2)*(pi^2))/3), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\frac{s^2\\pi^2}{3} = \\)", round(((input$scale_logistic^2)*(pi^2))/3, 3)))
    } else if (input$distribution == "Log-Normal") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\frac{1}{x\\sqrt{2\\pi \\sigma^2}}e^{-\\frac{1}{2\\sigma^2}(ln(x)-\\mu)^2} $$"),
        helpText("where \\( x > 0, -\\infty < \\mu < \\infty, \\sigma > 0\\)"),
        br(),
        helpText("\\(E(X) = e^{\\mu + \\frac{\\sigma^2}{2}} = \\)", round(exp(input$mean_lognormal + ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal/2, (input$sd_lognormal^2)/2)), 3)),
        helpText("\\(SD(X) = \\sqrt{(e^{\\sigma^2} - 1)e^{2\\mu + \\sigma^2}} = \\)", round(sqrt((exp(ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2))) - 1)*exp((2*input$mean_lognormal) + ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)))), 3)),
        helpText("\\(Var(X) = (e^{\\sigma^2} - 1)e^{2\\mu + \\sigma^2} = \\)", round((exp(ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2))) - 1)*exp((2*input$mean_lognormal) + ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2))), 3)))
    } else if (input$distribution == "Negative Binomial (I)") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = P(X = x) = \\binom{x+r-1}{r-1} (1-p)^x p^r $$"),
        helpText("where \\( x = 0, 1, 2, \\dots, r = 1, 2, \\dots \\) and \\( 0 < p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\frac{r(1-p)}{p} = \\)", round((input$r_negativebinomial*(1 - input$p_negativebinomial)/input$p_negativebinomial), 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\frac{r(1-p)}{p^2}} = \\)", round(sqrt((input$r_negativebinomial*(1 - input$p_negativebinomial)/(input$p_negativebinomial^2))), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\frac{r(1-p)}{p^2} = \\)", round((input$r_negativebinomial*(1 - input$p_negativebinomial)/(input$p_negativebinomial^2)), 3)))
    } else if (input$distribution == "Negative Binomial (II)") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = P(X = x) = \\binom{x-1}{r-1}p^r (1-p)^{x-r} $$"),
        helpText("where \\( x = r, r+1, \\dots \\) and \\( 0 < p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\frac{r}{p} = \\)", round((input$r_negativebinomial2/input$p_negativebinomial2), 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\frac{r(1-p)}{p^2}} = \\)", round(sqrt((input$r_negativebinomial2*(1 - input$p_negativebinomial2)/(input$p_negativebinomial2^2))), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\frac{r(1-p)}{p^2} = \\)", round((input$r_negativebinomial2*(1 - input$p_negativebinomial2)/(input$p_negativebinomial2^2)), 3)))
    } else if (input$distribution == "Normal") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\frac{1}{\\sqrt{2\\pi \\sigma^2}}e^{-\\frac{1}{2\\sigma^2}(x-\\mu)^2} $$"),
        helpText("where \\( -\\infty < x < \\infty, -\\infty < \\mu < \\infty, \\sigma > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\)", round(input$mean_normal, 3)),
        helpText("\\(\\sigma = SD(X) = \\)", ifelse(input$variance_sd == "variance_true", round(sqrt(input$variance_normal), 3), round(input$sd_normal, 3))),
        helpText("\\(\\sigma^2 = Var(X) = \\)", ifelse(input$variance_sd == "variance_true", round(input$variance_normal, 3), round(input$sd_normal^2, 3))))
    } else if (input$distribution == "Poisson") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = P(X = x) = \\frac{e^{-\\lambda}\\lambda^x}{x!} $$"),
        helpText("for \\( x = 0, 1, 2, \\dots\\)"),
        helpText("where \\( \\lambda > 0 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\lambda = \\)", round(input$lambda_poisson, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\lambda} = \\)", round(sqrt(input$lambda_poisson), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\lambda = \\)", round(input$lambda_poisson, 3)))
    } else if (input$distribution == "Student") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\frac{\\Gamma \\big(\\frac{df+1}{2}\\big)}{\\sqrt{df \\pi} \\Gamma \\big(\\frac{df}{2}\\big)} \\Big(1 + \\frac{x^2}{df}\\Big)^{-\\frac{df+1}{2}}$$"),
        helpText("where \\( -\\infty < x < \\infty, df > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\)", ifelse(input$df_student > 1, 0, "Undefined")),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\frac{df}{df - 2}} = \\)", ifelse(input$df_student > 2, round(sqrt(input$df_student / (input$df_student - 2)), 3), "Undefined")),
        helpText("\\(\\sigma^2 = Var(X) = \\frac{df}{df-2} = \\)", ifelse(input$df_student > 2, round(input$df_student / (input$df_student - 2), 3), "Undefined")))
    } else if (input$distribution == "Weibull") {
      withMathJax(
        helpText("Probability density function: $$ f(x) = \\frac{\\alpha}{\\beta} \\big(\\frac{x}{\\beta}\\big)^{\\alpha-1} e^{-(x / \\beta)^\\alpha} $$"),
        helpText("where \\( x > 0, \\alpha >0, \\beta > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\beta\\Gamma\\big(1 + \\frac{1}{\\alpha}\\big) = \\)", round(weibullparinv(shape = input$alpha_weibull, scale = input$beta_weibull, loc = 0)$mu, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\beta^2\\Big(\\Gamma\\big(1 + \\frac{2}{\\alpha}\\big) - \\Gamma\\big(1 + \\frac{1}{\\alpha}\\big)^2\\Big)} = \\)", round(weibullparinv(shape = input$alpha_weibull, scale = input$beta_weibull, loc = 0)$sigma, 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\beta^2\\Big(\\Gamma\\big(1 + \\frac{2}{\\alpha}\\big) - \\Gamma\\big(1 + \\frac{1}{\\alpha}\\big)^2\\Big) = \\)", round(weibullparinv(shape = input$alpha_weibull, scale = input$beta_weibull, loc = 0)$sigma^2, 3)))
    } else {
      print("in progress")
    }
  })
  
  # output$results_tests <- renderText({
  #   "in progress"
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

