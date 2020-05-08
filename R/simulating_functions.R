#' Create ideal data for a generalized linear model.
#'
#' @param N Sample size. (Default: 10000)
#' @param link Link function. See \code{\link[stats]{family}} for details.
#' @param weights Betas in glm model. See details. simulate_binomial: c(.1, .2) All other: c(1, 2, 3)
#' @param unrelated Number of unrelated features to return. (Default: 0)
#' @param ancillary Ancillary parameter for continuous families and negative binomial. See details.
#' @return A tibble with a response variable and predictors.
#' @details
#'
#' For many families, it is possible to pick weights that cause inverse link(X * weights) to be mathematically invalid.
#' For example, the log link for binomial regression defines P(Y=1) as exp(X * weights) which can be above one.
#' If this happens, the function will error with a helpful message.
#'
#' The intercept in the underlying link(Y) = X * weights + intercept is always max(weights). For example,
#' simulate_gaussian(link = "inverse", weights = 1:3) the model is (1/Y) = 1*X1 + 2*X2 + 3*X3 + 3.
#'
#'
#'  links
#'  \itemize{
#'   \item gaussian: identity, log, inverse
#'   \item binomial: logit, probit, cuachit, loglog, cloglog, log, logc
#'   \item gamma: inverse, identity, log
#'   \item poisson: log, identity, sqrt
#'   \item inverse gaussian: 1/mu^2, inverse, identity, log
#'   \item negative binomial: log, identity, sqrt
#'   \item tweedie: log, identity, sqrt, inverse
#'   }
#'  The default link is the first link listed for each family.
#'
#'
#'  ancillary parameter
#'  \itemize{
#'   \item gaussian: standard deviation
#'   \item binomial: N/A
#'   \item gamma: scale parameter
#'   \item poisson: N/A
#'   \item inverse gaussian: dispersion parameter
#'   \item negative binomial: theta.
#'   \item tweedie: rho
#'   }
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
  defaultAncillary = .3333
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
