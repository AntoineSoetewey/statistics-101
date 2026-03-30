# Contributing to statistics-101

Thank you for taking the time to consider contributing to this project.

This repository contains a Shiny app that helps students compute and visualize probabilities for common probability distributions.

As the maintainer, I truly benefit from contributions of all sizes. Whether you fix a bug, improve wording, suggest a better approach, or open an issue, your help is valuable and appreciated.

## Code of Conduct

By participating in this project, you agree to follow the code of conduct in [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).

## Ways to Contribute

- Share ideas or ask questions in an issue
- Report bugs or unexpected behavior
- Improve wording in the UI and documentation
- Add tests or validation checks for edge cases
- Refactor code to improve readability and maintainability
- Improve visuals and accessibility without changing core behavior unexpectedly

## Development Setup

### 1. Clone the repository

```bash
git clone https://github.com/AntoineSoetewey/statistics-101.git
cd statistics-101
```

### 2. Install dependencies

In R:

```r
install.packages(c("shiny", "shinycssloaders", "shinythemes", "dplyr", "ggplot2", "mixdist"))
```

### 3. Run the app locally

From the repository root, run:

```r
shiny::runApp()
```

You can also open `statistics-101.Rproj` in RStudio and launch the app from there.

## Project Structure

- `global.R`: package imports and global options
- `ui.R`: user interface layout and inputs
- `server.R`: reactive logic, probability calculations, and plots
- `www/css/styles.css`: custom styling
- `www/html/footer.html`: footer content

## Coding Guidelines

- Keep behavior consistent across distributions (input constraints, labels, and probability modes).
- Prefer clear and explicit code over compact but hard-to-read constructs.
- Follow the existing style in each file (naming, spacing, and structure).
- Avoid introducing unrelated refactors in the same pull request.
- Keep user-facing text concise and beginner-friendly.

## Validation Before Opening a PR

Please verify at least the following:

- The app starts locally without errors.
- The modified distribution(s) return expected values for representative inputs.
- Lower tail, upper tail, and interval modes still work where applicable.
- Plots render correctly and shaded areas match the selected probability mode.
- Any new or changed text is clear and typo-free.

If your change affects formulas or calculations, include at least one reproducible check in the PR description (for example, comparing output to base R functions like `pnorm()`, `pbinom()`, etc.).

## Pull Request Process

1. Create a feature branch from `master`.
2. Make focused commits with clear messages.
3. Open a pull request with:
	- A short summary of what changed
	- Why the change is needed
	- How you tested it
	- Screenshots or GIFs for UI changes (when relevant)
4. Link related issues if applicable.
5. Respond to review comments and keep discussion constructive.

## Commit Message Suggestions

Use short, action-oriented messages, for example:

- `Fix interval probability for geometric distribution`
- `Improve normal distribution input labels`
- `Refactor server logic for binomial plotting`
- `Update README with local setup instructions`

## Questions

If you are unsure whether a change fits the scope, please open an issue first and describe your proposal. I am happy to discuss ideas before you spend time implementing them.
