
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GlmSimulatoR

<!-- badges: start -->

<!-- badges: end -->

Often the first problem in understanding the generalized linear model in
a practical way is finding good data. Common problems in finding data
are a small amount of rows, the response variable does not follow a
family in the gm framework, or the data is messy and needs a lot of work
before statistical analysis can begin. This package alleviates all of
these by allowing you to create the data you want. With data in hand,
you can empirically answer any question you have.

The goal of this package is to strike a balance between mathematical
flexibility and simplicity of use. You can control the sample size, link
function, number of unrelated variables, and dispersion for continuous
distributions. Default values are carefully chosen so data can be
generated without thinking about mathematical connections between
weights, links, and distributions.

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

## Example

``` r
library(GlmSimulatoR)

#Do glm and lm estimate the same weights? Yes
set.seed(1)
simdata <- simulate_gaussian() #GlmSimulatoR function
linearModel <- lm(Y ~ X1 + X2 + X3, data = simdata)
glmModel <- glm(Y ~ X1 + X2 + X3, data = simdata, family = gaussian(link = "identity"))
summary(linearModel)
#> 
#> Call:
#> lm(formula = Y ~ X1 + X2 + X3, data = simdata)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.6961 -0.6711  0.0049  0.6534  3.6232 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  3.06105    0.08961   34.16   <2e-16 ***
#> X1           0.99941    0.03428   29.15   <2e-16 ***
#> X2           1.98930    0.03456   57.56   <2e-16 ***
#> X3           2.98383    0.03471   85.97   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.9976 on 9996 degrees of freedom
#> Multiple R-squared:  0.5377, Adjusted R-squared:  0.5375 
#> F-statistic:  3875 on 3 and 9996 DF,  p-value: < 2.2e-16
summary(glmModel)
#> 
#> Call:
#> glm(formula = Y ~ X1 + X2 + X3, family = gaussian(link = "identity"), 
#>     data = simdata)
#> 
#> Deviance Residuals: 
#>     Min       1Q   Median       3Q      Max  
#> -3.6961  -0.6711   0.0049   0.6534   3.6232  
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  3.06105    0.08961   34.16   <2e-16 ***
#> X1           0.99941    0.03428   29.15   <2e-16 ***
#> X2           1.98930    0.03456   57.56   <2e-16 ***
#> X3           2.98383    0.03471   85.97   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for gaussian family taken to be 0.9952888)
#> 
#>     Null deviance: 21518.1  on 9999  degrees of freedom
#> Residual deviance:  9948.9  on 9996  degrees of freedom
#> AIC: 28338
#> 
#> Number of Fisher Scoring iterations: 2
rm(linearModel, glmModel, simdata)
```

## See Vignettes For More Examples
