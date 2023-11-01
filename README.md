# VIBE
<!-- badges: start -->
[![R-CMD-check](https://github.com/Stan125/vibe/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Stan125/vibe/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
THe acronym **vibe** stands for **V**ariable **I**mportance **BE**yond linear models. Significance assessments of single variables are part of every classical regression modeling toolset, but they fail at displaying which variables contribute the most in explaining the target variable, since comparisons cannot reasonably be made. The software package **vibe** implements two different methods for ranking the impact of our explanatory variables on the dependent variable ´y´:

- Hierarchical Partitioning (Chevan & Sutherland, 1991)
- Relative Weights (Johnson, 2000)

To obtain the first metric, all submodels are computed and the average reduction/increase in the goodness of fit is calculated. This is the most complete metric and gives the option to use non-linear effects like penalized splines etc. Relative Weights is an approximation to Hierarchical Partitioning which is useful in cases with lots of covariates, since it is much faster. The approximation works through orthogonalization of the design matrix which is then used for an orthogonal model. The results of the orthogonal model are then reweighted using standardized coefficients of the original model.

