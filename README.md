# statistics-101

A Shiny app to compute probabilities for the main probability distributions.

**Live app:** https://antoinesoetewey.shinyapps.io/statistics-101/

**Companion guide:** https://statsandr.com/blog/a-guide-on-how-to-read-statistical-tables/

---

## Purpose and Overview

Statistics 101 is an interactive web application built with R Shiny that allows users to compute probabilities for a wide range of probability distributions. It is designed as a digital alternative to traditional statistical tables, enabling students to look up and visualize probabilities without needing printed reference tables.

Users select a distribution, set its parameters, choose a probability type (lower tail, upper tail, or interval), and instantly receive the exact probability alongside a plot of the distribution with the relevant area highlighted.

---

## Key Features

- **18 supported distributions** — both discrete and continuous:
  - Discrete: Binomial, Geometric (I), Geometric (II), Hypergeometric, Negative Binomial (I), Negative Binomial (II), Poisson
  - Continuous: Beta, Cauchy, Chi-square, Exponential, Fisher, Gamma, Logistic, Log-Normal, Normal, Student, Weibull
- **Three probability types** for each distribution:
  - Lower tail: P(X ≤ x)
  - Upper tail: P(X > x)
  - Interval: P(a ≤ X ≤ b)
- **Interactive parameter inputs** — distribution parameters update in real time, with constraints to prevent invalid values
- **LaTeX-rendered output** — probability results are displayed using MathJax notation (e.g., X ~ N(μ = 0, σ² = 1) and P(X ≤ 1.96) = 0.975)
- **Distribution plots** — each query is accompanied by a ggplot2 visualization with the probability area shaded
- **Flexible parameterization** — for distributions like Normal and Log-Normal, users can specify either variance or standard deviation

---

## Running the App Locally

### Prerequisites

Install R (>= 4.0 recommended) and the following packages:

```r
install.packages(c("shiny", "shinycssloaders", "shinythemes", "dplyr", "ggplot2", "mixdist"))
```

### Launch

Clone the repository and run the app from the project root:

```r
# In R or RStudio
shiny::runApp()
```

Or from the terminal:

```bash
Rscript -e "shiny::runApp()"
```

The app will open in your default browser at `http://127.0.0.1:<port>`.

---

## Dependencies

| Package | Role |
|---|---|
| `shiny` | Web application framework |
| `shinycssloaders` | Loading spinners for reactive outputs |
| `shinythemes` | UI theme (Flatly) |
| `dplyr` | Data manipulation |
| `ggplot2` | Distribution plots |
| `mixdist` | Supporting distribution utilities |

---

## Related Apps

This app is part of a set of three complementary Shiny applications developed for students while I was a teaching assistant at UCLouvain. All three apps are still actively used in introductory statistics and probability courses.

- **statistics-101** — compute probabilities for the main probability distributions: https://github.com/AntoineSoetewey/statistics-101
- **statistics-201** — perform statistical inference on mean(s), proportion(s), and variance(s): https://github.com/AntoineSoetewey/statistics-201
- **statistics-202** — simple linear regression by hand: https://github.com/AntoineSoetewey/statistics-202

---

## License

This project is licensed under the terms of the Creative Commons Attribution 4.0 International License (CC BY 4.0). See [LICENSE](LICENSE) for more details.
