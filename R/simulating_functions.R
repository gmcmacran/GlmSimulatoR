#' Create ideal data for a generalized linear model.
#'
#' @param N Sample size. (Default: 10000)
#' @param link Link function. See \code{\link[stats]{family}} for details.
#' @param weights Betas in glm model. See details. simulate_binomial: c(.1, .2) All other: c(1, 2, 3)
#' @param unrelated Number of unrelated features to return. (Default: 0)
#' @param ancillary Ancillary parameter for continuous families and negative binomial. See details.
#' @return A tibble with a response variable and predictors.
#' @details
#' The gaussian family accepts the links identity, log and inverse.
#' The binomial family accepts the links logit, probit, cauchit,
#' (corresponding to logistic, normal and Cauchy CDFs respectively)
#' log and cloglog (complementary log-log).
#' The gamma family accepts the links inverse, identity and log.
#' The poisson family accepts the links log, identity, and sqrt.
#' The inverse gaussian family accepts the links 1/mu^2, inverse, identity and log.
#' The negative binomial family accepts the links log, identity, and sqrt using glm.nb in MASS.
#' The tweedie family accepts the links log, identity, sqrt and inverse.
#'
#' Default links are identity for gaussian, logit for binomial, inverse for gamma, log for poisson,
#' 1/mu^2 for inverse gaussian, log for negative binomial and log for tweedie.
#'
#' It is possible to pick weights that cause inverse link(X * weights) to be mathematically invalid.
#' For example, the log link for binomial regression defines P(Y=1) as exp(X * weights) which can be above one.
#' If this happens, the function will error with a helpful message. For P(Y=1) to be between zero and one,
#' weights should be small. In general, the log link is the most troublesome to work with. It is recommended to use weights
#' in the neighborhood of .1 to .3 for log link.
#'
#' For the inverse gaussian family, the inverse link function needs weights*X to be positive.
#'
#' The intercept in the underlying link(Y) = X * weights + intercept is always max(weights). For example,
#' simulate_gaussian(link = "inverse", weights = 1:3) the model is (1/Y) = 1*X1 + 2*X2 + 3*X3 + 3.
#'
#' The continuous families have an ancillary parameter. For the gaussian family, it is standard deviation.
#' Default value is 1. For the gamma family, it is the scale parameter. Default value is .05. For inverse gaussian,
#' it is the dispersion parameter. Default value 1/3. For negative binomial it is theta. Default value 1. For
#' tweedie, it is rho. Default value 1.15. Not used in binomial and poisson.
#'
#' @examples
#' library(GlmSimulatoR)
#' library(ggplot2)
#' library(MASS)
#' 
#' # Do glm and lm estimate the same weights? Yes
#' set.seed(1)
#' simdata <- simulate_gaussian()
#' linearModel <- lm(Y ~ X1 + X2 + X3, data = simdata)
#' glmModel <- glm(Y ~ X1 + X2 + X3, data = simdata, family = gaussian(link = "identity"))
#' summary(linearModel)
#' summary(glmModel)
#' rm(linearModel, glmModel, simdata)
#' 
#' # If the effects are multiplicative instead of additive,
#' # will my response variable still be normal? Yes
#' set.seed(1)
#' simdata <- simulate_gaussian(N = 1000, link = "log", weights = c(.1, .2))
#' 
#' ggplot(simdata, aes(x = Y)) +
#'   geom_histogram(bins = 30)
#' rm(simdata)
#' 
#' # Is AIC lower for the correct link? For ten thousand data points, depends on seed!
#' set.seed(1)
#' simdata <- simulate_gaussian(N = 10000, link = "inverse", weights = 1)
#' glmCorrectLink <- glm(Y ~ X1, data = simdata, family = gaussian(link = "inverse"))
#' glmWrongLink <- glm(Y ~ X1, data = simdata, family = gaussian(link = "identity"))
#' summary(glmCorrectLink)$aic
#' summary(glmWrongLink)$aic
#' rm(simdata, glmCorrectLink, glmWrongLink)
#' 
#' 
#' # Does a stepwise search find the correct model for logistic regression? Yes
#' # 3 related variables. 3 unrelated variables.
#' set.seed(1)
#' simdata <- simulate_binomial(N = 10000, link = "logit", weights = c(.3, .4, .5), unrelated = 3)
#' 
#' scopeArg <- list(
#'   lower = Y ~ 1,
#'   upper = Y ~ X1 + X2 + X3 + Unrelated1 + Unrelated2 + Unrelated3
#' )
#' 
#' startingModel <- glm(Y ~ 1, data = simdata, family = binomial(link = "logit"))
#' glmModel <- stepAIC(startingModel, scopeArg)
#' summary(glmModel)
#' rm(simdata, scopeArg, startingModel, glmModel)
#' 
#' # When the resposne is a gamma distribution, what does a scatter plot between X and Y look like?
#' set.seed(1)
#' simdata <- simulate_gamma(weights = 1)
#' ggplot(simdata, aes(x = X1, y = Y)) +
#'   geom_point()
#' rm(simdata)
#' @export
simulate_gaussian <- make_simulating_function(
  validLinks = c("identity", "log", "inverse"),
  defaultLink = "identity",
  defaultWeights = 1:3,
  make_response = create_gaussian,
  defaultAncillary = 1
)

#' @rdname simulate_gaussian
#' @export
simulate_binomial <- make_simulating_function(
  validLinks = c("logit", "probit", "cauchit", "log", "cloglog", "loglog", "logc", "identity"),
  defaultLink = "logit",
  defaultWeights = c(.1, .2),
  make_response = GlmSimulatoR:::create_binomial,
  defaultAncillary = NULL
)

#' @rdname simulate_gaussian
#' @export
simulate_gamma <- make_simulating_function(
  validLinks = c("inverse", "identity", "log"),
  defaultLink = "inverse",
  defaultWeights = 1:3,
  make_response = GlmSimulatoR:::create_gamma,
  defaultAncillary = .05
)

#' @rdname simulate_gaussian
#' @export
simulate_poisson <- make_simulating_function(
  validLinks = c("log", "identity", "sqrt"),
  defaultLink = "log",
  defaultWeights = c(.5, 1),
  make_response = GlmSimulatoR:::create_poisson,
  defaultAncillary = NULL
)

#' @rdname simulate_gaussian
#' @export
simulate_inverse_gaussian <- make_simulating_function(
  validLinks = c("1/mu^2", "inverse", "identity", "log"),
  defaultLink = "1/mu^2",
  defaultWeights = 1:3,
  make_response = GlmSimulatoR:::create_inverse_gaussian,
  defaultAncillary = 1 / 3
)

#' @rdname simulate_gaussian
#' @export
simulate_negative_binomial <- make_simulating_function(
  validLinks = c("log", "identity", "sqrt"),
  defaultLink = "log",
  defaultWeights = c(.5, 1),
  make_response = GlmSimulatoR:::create_negative_binomial,
  defaultAncillary = 1
)

#' @rdname simulate_gaussian
#' @export
simulate_tweedie <- make_simulating_function(
  validLinks = c("log", "identity", "sqrt", "inverse"),
  defaultLink = "log",
  defaultWeights = c(.02),
  make_response = GlmSimulatoR:::create_tweedie,
  defaultAncillary = 1.15
)
