ui <- shiny::tagList(
  withMathJax(), 
  includeCSS(path = "www/css/styles.css"), 
  
  tags$div(
    tags$div(
      class = "app_title", 
      
      titlePanel(
        title = "Statistics 101 - Probability distributions", 
        windowTitle = "Probability Distributions"
      ),
      
      tags$h4(
        tags$a(
          href = "https://antoinesoetewey.com/", 
          "Antoine Soetewey"
        )
      )
    ),
    
    # Sidebar with a slider input for number of bins
    fluidPage(
      theme = shinythemes::shinytheme("yeti"),
      sidebarLayout(
        sidebarPanel(
          width = 4, 
          
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
                         value = 1, min = 0, step = 1
            ),
            numericInput("beta_beta", "Shape \\(\\beta\\):",
                         value = 3, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Binomial'",
            numericInput("n_binomial", "Number of trials \\(n\\):",
                         value = 20, min = 0, step = 1
            ),
            numericInput("p_binomial", "Probability of success \\(p\\):",
                         value = 0.5, min = 0, max = 1, step = 0.01
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Cauchy'",
            numericInput("location_cauchy", "Location \\(x_0\\):",
                         value = 0, step = 1
            ),
            numericInput("scale_cauchy", "Scale \\(\\gamma\\):",
                         value = 1, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Chi-square'",
            numericInput("df_chisquare", "Degrees of freedom \\(df\\):",
                         value = 6, min = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Exponential'",
            numericInput("rate_exponential", "Rate \\(\\lambda\\):",
                         value = 1, min = 0, step = 0.5
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Fisher'",
            numericInput("df1_fisher", "Degrees of freedom \\(df_1\\):",
                         value = 10, min = 1, step = 1
            ),
            numericInput("df2_fisher", "Degrees of freedom \\(df_2\\):",
                         value = 5, min = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Gamma'",
            numericInput("alpha_gamma", "Shape \\(\\alpha\\):",
                         value = 3, min = 0, step = 1
            ),
            numericInput("beta_gamma", "Rate \\(\\beta\\):",
                         value = 2, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Geometric (I)'",
            numericInput("p_geometric", "Probability of success \\(p\\):",
                         value = 0.5, min = 0, max = 1, step = 0.01
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Geometric (II)'",
            numericInput("p_geometric2", "Probability of success \\(p\\):",
                         value = 0.5, min = 0, max = 1, step = 0.01
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Hypergeometric'",
            numericInput("n_hypergeometric", "Sample size \\(n\\):",
                         value = 100, min = 0, step = 1
            ),
            numericInput("N_hypergeometric", "Total number of objects \\(N\\):",
                         value = 500, min = 0, step = 1
            ),
            numericInput("M_hypergeometric", "Number of successes \\(M\\):",
                         value = 50, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Logistic'",
            numericInput("location_logistic", "Location \\(\\mu\\):",
                         value = 0, step = 1
            ),
            numericInput("scale_logistic", "Scale \\(s\\):",
                         value = 1, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Log-Normal'",
            numericInput("mean_lognormal", "Mean \\(\\mu\\):",
                         value = 0, step = 1
            ),
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
                         value = 1, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Log-Normal' && input.variance_sd_lognormal == 'variance_false'",
            numericInput("sd_lognormal", "Standard deviation \\(\\sigma\\):",
                         value = 1, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Negative Binomial (I)'",
            numericInput("r_negativebinomial", "Number of successes \\(r\\):",
                         value = 5, min = 1, step = 1
            ),
            numericInput("p_negativebinomial", "Probability of success \\(p\\):",
                         value = 0.5, min = 0, max = 1, step = 0.01
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Negative Binomial (II)'",
            numericInput("r_negativebinomial2", "Number of successes \\(r\\):",
                         value = 5, min = 1, step = 1
            ),
            numericInput("p_negativebinomial2", "Probability of success \\(p\\):",
                         value = 0.5, min = 0, max = 1, step = 0.01
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Normal'",
            numericInput("mean_normal", "Mean \\(\\mu\\):",
                         value = 0, step = 1
            ),
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
                         value = 1, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Normal' && input.variance_sd == 'variance_false'",
            numericInput("sd_normal", "Standard deviation \\(\\sigma\\):",
                         value = 1, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Poisson'",
            numericInput("lambda_poisson", "Rate \\(\\lambda\\):",
                         value = 4, min = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Student'",
            numericInput("df_student", "Degrees of freedom \\(df\\):",
                         value = 10, min = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Weibull'",
            numericInput("alpha_weibull", "Shape \\(\\alpha\\):",
                         value = 5, min = 0, step = 1
            ),
            numericInput("beta_weibull", "Scale \\(\\beta\\):",
                         value = 1, min = 0, step = 1
            )
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
                         value = 0.45, min = 0, max = 1, step = 0.01
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Beta' && input.lower_tail_beta == 'upper.tail'",
            numericInput("x2_beta", "x:",
                         value = 0.45, min = 0, max = 1, step = 0.01
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Beta' && input.lower_tail_beta == 'interval'",
            numericInput("a_beta", "a:",
                         value = 0.25, min = 0, max = 1, step = 0.01
            ),
            numericInput("b_beta", "b: \\( (a \\leq b) \\)",
                         value = 0.45, min = 0, max = 1, step = 0.01
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Binomial' && input.lower_tail_binomial == 'lower.tail'",
            numericInput("x1_binomial", "x:",
                         value = 8, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Binomial' && input.lower_tail_binomial == 'upper.tail'",
            numericInput("x2_binomial", "x:",
                         value = 8, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Binomial' && input.lower_tail_binomial == 'interval'",
            numericInput("a_binomial", "a:",
                         value = 8, min = 0, step = 1
            ),
            numericInput("b_binomial", "b: \\( (a \\leq b) \\)",
                         value = 12, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Cauchy' && input.lower_tail_cauchy == 'lower.tail'",
            numericInput("x1_cauchy", "x:",
                         value = 1.2, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Cauchy' && input.lower_tail_cauchy == 'upper.tail'",
            numericInput("x2_cauchy", "x:",
                         value = 1.2, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Cauchy' && input.lower_tail_cauchy == 'interval'",
            numericInput("a_cauchy", "a:",
                         value = -1.2, step = 1
            ),
            numericInput("b_cauchy", "b: \\( (a \\leq b) \\)",
                         value = 1.2, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Chi-square' && input.lower_tail_chisquare == 'lower.tail'",
            numericInput("x1_chisquare", "x:",
                         value = 9.6, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Chi-square' && input.lower_tail_chisquare == 'upper.tail'",
            numericInput("x2_chisquare", "x:",
                         value = 9.6, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Chi-square' && input.lower_tail_chisquare == 'interval'",
            numericInput("a_chisquare", "a:",
                         value = 9.6, min = 0, step = 1
            ),
            numericInput("b_chisquare", "b: \\( (a \\leq b) \\)",
                         value = 14.4, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Exponential' && input.lower_tail_exponential == 'lower.tail'",
            numericInput("x1_exponential", "x:",
                         value = 2.24, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Exponential' && input.lower_tail_exponential == 'upper.tail'",
            numericInput("x2_exponential", "x:",
                         value = 2.24, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Exponential' && input.lower_tail_exponential == 'interval'",
            numericInput("a_exponential", "a:",
                         value = 2.24, min = 0, step = 1
            ),
            numericInput("b_exponential", "b: \\( (a \\leq b) \\)",
                         value = 3.36, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Fisher' && input.lower_tail_fisher == 'lower.tail'",
            numericInput("x1_fisher", "x:",
                         value = 4.14, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Fisher' && input.lower_tail_fisher == 'upper.tail'",
            numericInput("x2_fisher", "x:",
                         value = 4.14, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Fisher' && input.lower_tail_fisher == 'interval'",
            numericInput("a_fisher", "a:",
                         value = 2.76, min = 0, step = 1
            ),
            numericInput("b_fisher", "b: \\( (a \\leq b) \\)",
                         value = 4.14, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Gamma' && input.lower_tail_gamma == 'lower.tail'",
            numericInput("x1_gamma", "x:",
                         value = 2.4, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Gamma' && input.lower_tail_gamma == 'upper.tail'",
            numericInput("x2_gamma", "x:",
                         value = 2.4, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Gamma' && input.lower_tail_gamma == 'interval'",
            numericInput("a_gamma", "a:",
                         value = 0.8, min = 0, step = 1
            ),
            numericInput("b_gamma", "b: \\( (a \\leq b) \\)",
                         value = 2.4, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Geometric (I)' && input.lower_tail_geometric == 'lower.tail'",
            helpText("Number of failures before the \\(1^{st}\\) success"),
            numericInput("x1_geometric", "x:",
                         value = 1, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Geometric (I)' && input.lower_tail_geometric == 'upper.tail'",
            helpText("Number of failures before the \\(1^{st}\\) success"),
            numericInput("x2_geometric", "x:",
                         value = 1, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Geometric (I)' && input.lower_tail_geometric == 'interval'",
            helpText("Number of failures before the \\(1^{st}\\) success"),
            numericInput("a_geometric", "a:",
                         value = 1, min = 0, step = 1
            ),
            numericInput("b_geometric", "b: \\( (a \\leq b) \\)",
                         value = 3, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Geometric (II)' && input.lower_tail_geometric2 == 'lower.tail'",
            helpText("The trial on which the \\(1^{st}\\) success occurs"),
            numericInput("x1_geometric2", "x:",
                         value = 2, min = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Geometric (II)' && input.lower_tail_geometric2 == 'upper.tail'",
            helpText("The trial on which the \\(1^{st}\\) success occurs"),
            numericInput("x2_geometric2", "x:",
                         value = 2, min = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Geometric (II)' && input.lower_tail_geometric2 == 'interval'",
            helpText("The trial on which the \\(1^{st}\\) success occurs"),
            numericInput("a_geometric2", "a:",
                         value = 2, min = 1, step = 1
            ),
            numericInput("b_geometric2", "b: \\( (a \\leq b) \\)",
                         value = 4, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Hypergeometric' && input.lower_tail_hypergeometric == 'lower.tail'",
            numericInput("x1_hypergeometric", "x:",
                         value = 8, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Hypergeometric' && input.lower_tail_hypergeometric == 'upper.tail'",
            numericInput("x2_hypergeometric", "x:",
                         value = 8, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Hypergeometric' && input.lower_tail_hypergeometric == 'interval'",
            numericInput("a_hypergeometric", "a:",
                         value = 8, min = 0, step = 1
            ),
            numericInput("b_hypergeometric", "b: \\( (a \\leq b) \\)",
                         value = 12, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Logistic' && input.lower_tail_logistic == 'lower.tail'",
            numericInput("x1_logistic", "x:",
                         value = 1.2, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Logistic' && input.lower_tail_logistic == 'upper.tail'",
            numericInput("x2_logistic", "x:",
                         value = 1.2, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Logistic' && input.lower_tail_logistic == 'interval'",
            numericInput("a_logistic", "a:",
                         value = -1.2, step = 1
            ),
            numericInput("b_logistic", "b: \\( (a \\leq b) \\)",
                         value = 1.2, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Log-Normal' && input.lower_tail_lognormal == 'lower.tail'",
            numericInput("x1_lognormal", "x:",
                         value = 1, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Log-Normal' && input.lower_tail_lognormal == 'upper.tail'",
            numericInput("x2_lognormal", "x:",
                         value = 1, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Log-Normal' && input.lower_tail_lognormal == 'interval'",
            numericInput("a_lognormal", "a:",
                         value = 1, min = 0, step = 1
            ),
            numericInput("b_lognormal", "b: \\( (a \\leq b) \\)",
                         value = 2, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Negative Binomial (I)' && input.lower_tail_negativebinomial == 'lower.tail'",
            helpText("Number of failures before the \\(r^{th}\\) success"),
            numericInput("x1_negativebinomial", "x:",
                         value = 2, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Negative Binomial (I)' && input.lower_tail_negativebinomial == 'upper.tail'",
            helpText("Number of failures before the \\(r^{th}\\) success"),
            numericInput("x2_negativebinomial", "x:",
                         value = 2, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Negative Binomial (I)' && input.lower_tail_negativebinomial == 'interval'",
            helpText("Number of failures before the \\(r^{th}\\) success"),
            numericInput("a_negativebinomial", "a:",
                         value = 2, min = 0, step = 1
            ),
            numericInput("b_negativebinomial", "b: \\( (a \\leq b) \\)",
                         value = 4, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Negative Binomial (II)' && input.lower_tail_negativebinomial2 == 'lower.tail'",
            helpText("The trial on which the \\(r^{th}\\) success occurs"),
            numericInput("x1_negativebinomial2", "x: \\( (x \\geq r) \\)",
                         value = 7, min = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Negative Binomial (II)' && input.lower_tail_negativebinomial2 == 'upper.tail'",
            helpText("The trial on which the \\(r^{th}\\) success occurs"),
            numericInput("x2_negativebinomial2", "x: \\( (x \\geq r) \\)",
                         value = 7, min = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Negative Binomial (II)' && input.lower_tail_negativebinomial2 == 'interval'",
            helpText("The trial on which the \\(r^{th}\\) success occurs"),
            numericInput("a_negativebinomial2", "a: \\( (a \\geq r) \\)",
                         value = 7, min = 1, step = 1
            ),
            numericInput("b_negativebinomial2", "b: \\( (r \\leq a \\leq b) \\)",
                         value = 9, min = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Normal' && input.lower_tail_normal == 'lower.tail'",
            numericInput("x1_normal", "x:",
                         value = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Normal' && input.lower_tail_normal == 'upper.tail'",
            numericInput("x2_normal", "x:",
                         value = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Normal' && input.lower_tail_normal == 'interval'",
            numericInput("a_normal", "a:",
                         value = -1, step = 1
            ),
            numericInput("b_normal", "b: \\( (a \\leq b) \\)",
                         value = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Poisson' && input.lower_tail_poisson == 'lower.tail'",
            numericInput("x1_poisson", "x:",
                         value = 6, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Poisson' && input.lower_tail_poisson == 'upper.tail'",
            numericInput("x2_poisson", "x:",
                         value = 6, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Poisson' && input.lower_tail_poisson == 'interval'",
            numericInput("a_poisson", "a:",
                         value = 6, min = 0, step = 1
            ),
            numericInput("b_poisson", "b: \\( (a \\leq b) \\)",
                         value = 10, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Student' && input.lower_tail_student == 'lower.tail'",
            numericInput("x1_student", "x:",
                         value = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Student' && input.lower_tail_student == 'upper.tail'",
            numericInput("x2_student", "x:",
                         value = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Student' && input.lower_tail_student == 'interval'",
            numericInput("a_student", "a:",
                         value = -1, step = 1
            ),
            numericInput("b_student", "b: \\( (a \\leq b) \\)",
                         value = 1, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Weibull' && input.lower_tail_weibull == 'lower.tail'",
            numericInput("x1_weibull", "x:",
                         value = 0.8, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Weibull' && input.lower_tail_weibull == 'upper.tail'",
            numericInput("x2_weibull", "x:",
                         value = 0.8, min = 0, step = 1
            )
          ),
          conditionalPanel(
            condition = "input.distribution == 'Weibull' && input.lower_tail_weibull == 'interval'",
            numericInput("a_weibull", "a:",
                         value = 0.8, min = 0, step = 1
            ),
            numericInput("b_weibull", "b: \\( (a \\leq b) \\)",
                         value = 1.2, min = 0, step = 1
            )
          )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          width = 8, 
          
          br(),
          tags$b("Solution:"),
          uiOutput("results_distribution"),
          br(),
          
          # ----plots----
          plotOutput(outputId = "plots") |> 
            shinycssloaders::withSpinner(
              type = 2, 
              color.background = "white"
            ), 
          
          br(),
          tags$b("Details:"),
          br(),
          
          # ----details----
          uiOutput("parameters_distribution") |> 
            shinycssloaders::withSpinner(
              type = 2, 
              color.background = "white"
            ), 
          
          br(),
          br()
        )
      )
      
    )      
  ), 
  
  tags$footer(
    tags$div(
      class = "footer_container", 
      
      includeHTML(path = "www/html/footer.html")
    )
  )
  
)
