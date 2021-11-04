
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GlmSimulatoR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/GlmSimulatoR)](https://cran.r-project.org/package=GlmSimulatoR)
[![R build
status](https://github.com/gmcmacran/GlmSimulatoR/workflows/R-CMD-check/badge.svg)](https://github.com/gmcmacran/GlmSimulatoR/actions)
[![Codecov test
coverage](https://codecov.io/gh/gmcmacran/GlmSimulatoR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/gmcmacran/GlmSimulatoR?branch=master)
<!-- badges: end -->

Often the first problem in understanding statistical models is finding
good data. This package alleviates this by creating data perfect for
generalized linear models.

With data in hand, you can focus on questions about models instead of
questions about data. Are the estimated weights close to the true
values? Does step wise search pick the correct variables? At what n does
the sampling distribution of weights normalize?

## Package Overview

All functions return a tibble. The only thing that changes is the
distribution of Y. In simulate_gaussian, Y follows a Gaussian
distribution. In simulate_gamma, Y follows a gamma distribution. Common
and novel distributions are implemented. For each distribution, all
links are implemented.

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
the generalized linear model worked well.

See vignettes for more examples.
