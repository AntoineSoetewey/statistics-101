# statistics-101 NEWS

## v1.0.1 (2026-05-26)

- Rewrote unit tests to be application-specific: tests now target the app's
  custom helper functions (`cdf_continuous`, `cdf_discrete`) and the
  non-trivial probability reparameterizations in `server.R` (Geometric II,
  Negative Binomial II, and discrete interval adjustments)

## v1.0.0 (2026-05-26)

First release, incorporating changes from the JOSE peer review.

- Increased PDF/PMF curve smoothness (#7)
- Improved MathJax formatting in the Details panel (#8)
- Added link to the live app in the sidebar (#9)
- Added CDF toggle (PDF/PMF ↔ CDF) for all 18 distributions (#10)
- Added skewness and excess kurtosis to the Details panel for all distributions (#11)
- Added unit tests covering CDF helper functions and probability formula correctness (#12)
