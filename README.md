
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GlmSimulatoR

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/gmcmacran/GlmSimulatoR.svg?branch=master)](https://travis-ci.org/gmcmacran/GlmSimulatoR)
[![CRAN
status](https://www.r-pkg.org/badges/version/GlmSimulatoR)](https://cran.r-project.org/package=GlmSimulatoR)
[![Codecov test
coverage](https://codecov.io/gh/gmcmacran/GlmSimulatoR/branch/master/graph/badge.svg)](https://codecov.io/gh/gmcmacran/GlmSimulatoR?branch=master)
<!-- badges: end -->

Often the first problem in understanding the generalized linear model in
a practical way is finding good data. Common problems include finding
data with a small number of rows, the response variable does not follow
a family in the glm framework, or the data is messy and needs a lot of
work before statistical analysis can begin. This package alleviates all
of these by allowing you to create the data you want. With data in hand,
you can empirically answer any question you have.

The goal of this package is to strike a balance between mathematical
flexibility and simplicity of use. You can control the sample size, link
function, number of unrelated variables, and ancillary parameter when
applicable. Default values are carefully chosen so data can be generated
without thinking about mathematical connections between weights, links,
and distributions.

## Installation

You can install the released version of GlmSimulatoR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("GlmSimulatoR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gmcmacran/GlmSimulatoR")
```

## Example 1: Create Textbook Data for Linear Model

``` r
library(GlmSimulatoR)
library(ggplot2)

set.seed(1)
simdata <- simulate_gaussian(N = 100, weights = 1, xrange = 10, ancillary = 1) #GlmSimulatoR function
ggplot(simdata, aes(x = X1, y = Y)) + 
  geom_point()
```

<img src="man/figures/README-example1-1.png" width="100%" />

``` r

rm(simdata)
```

## Example 2: Do glm and lm Estimate The Same Weights?

``` r
library(GlmSimulatoR)

set.seed(1)
simdata <- simulate_gaussian() #GlmSimulatoR function
linearModel <- lm(Y ~ X1 + X2 + X3, data = simdata)
glmModel <- glm(Y ~ X1 + X2 + X3, data = simdata, family = gaussian(link = "identity"))
summary(linearModel)$coefficients
#>              Estimate Std. Error  t value      Pr(>|t|)
#> (Intercept) 3.0610462 0.08961157 34.15905 5.565886e-242
#> X1          0.9994106 0.03428367 29.15122 2.238522e-179
#> X2          1.9892954 0.03455924 57.56189  0.000000e+00
#> X3          2.9838318 0.03470746 85.97092  0.000000e+00
summary(glmModel)$coefficients
#>              Estimate Std. Error  t value      Pr(>|t|)
#> (Intercept) 3.0610462 0.08961157 34.15905 5.565886e-242
#> X1          0.9994106 0.03428367 29.15122 2.238522e-179
#> X2          1.9892954 0.03455924 57.56189  0.000000e+00
#> X3          2.9838318 0.03470746 85.97092  0.000000e+00
rm(linearModel, glmModel, simdata)
```

In the summary, the weights and standard errors are the same.

## See Vignettes For More Examples
