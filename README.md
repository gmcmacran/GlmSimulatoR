
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GlmSimulatoR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/GlmSimulatoR)](https://cran.r-project.org/package=GlmSimulatoR)
[![R-CMD-check](https://github.com/gmcmacran/GlmSimulatoR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gmcmacran/GlmSimulatoR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/gmcmacran/GlmSimulatoR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gmcmacran/GlmSimulatoR?branch=main)
<!-- badges: end -->

## Overview

Understating statistical models is difficult. Experimentation on
**models** should be a part of the learning process. This package
provides functions that generate ideal data for generalized linear
models. Model parameters, link functions, sample size, and more are
adjustable. With data controlled, models can be experimented on.

## Is a sample size of 200 enough to get close estimates of the true weights?

``` r
library(GlmSimulatoR)

set.seed(1)
simdata <- simulate_gaussian(N = 200, weights = c(1, 2, 3))

model <- lm(Y ~ X1 + X2 + X3, data = simdata)
summary(model)$coefficients
#>              Estimate Std. Error   t value     Pr(>|t|)
#> (Intercept) 2.9138043  0.7011699  4.155633 4.843103e-05
#> X1          0.9833586  0.2868396  3.428253 7.403616e-04
#> X2          1.7882468  0.2701817  6.618683 3.386406e-10
#> X3          3.2822020  0.2640478 12.430334 1.550439e-26
```

The estimates are close to the weights argument. The mathematics behind
the linear model worked well.

## Addititional Examples in Vignettes

- Count data and over dispersion
- Dealing with right skewed data
- Exploring links for the Gaussian distribution
- Stepwise Search
- Tweedie distribution.

Foo
