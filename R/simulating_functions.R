#' Create ideal data for a glm model.
#'
#' @param N Sample size. (Default: 10000)
#' @param link Link function. See \code{\link[stats]{family}} for list of links.
#' @param weights Slopes in glm model. See details. simulate_binomial: c(.1, .2) All other: c(1, 2, 3)
#' @param unrelated Number of unrelated features to return. (Default: 0)
#' @param dispersion  Dispersion parameter for family where applicable.
#' @return A tibble with a response variable and predictors.
#' @details The default value for argument weights works well for all link family combinations.
#' The functions also validate input and provide helpfull error messages. Mistakes like passing a
#' link of "1/mu^2 to the gaussion function will error.
#'
#' It is possible to pick weights that cause inverse link(X * weights) to be mathematically invalid.
#' For example, the log link for binomial regression defines P(Y=1) as exp(X * weights). For P(Y=1) to
#' be between zero and one, weights should be small.
#'
#' For inverse gaussion, the inverse of the default link function needs weights*X to be postive.
#'
#'
#' The intercept in the underlying link(Y) = X * weights + intercept is always max(weights). For example,
#' simulate_gaussian(link = "inverse", weights = 1:3) the model is (1/Y) = 1*X1 + 2*X2 + 3*X3 + 3.
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
#' simdata <- simulate_gaussian(N = 1000, link = "log", weights = .1)
#'
#' ggplot(simdata, aes(x = Y)) +
#'   geom_histogram(bins = 30)
#' rm(simdata)
#'
#' # Is AIC lower for the correct link? For ten thousand data points, depends on seed!
#' # For larger N, AIC is lower.
#' set.seed(1)
#' simdata <- simulate_gaussian(N = 10000, link = "inverse", weights = 1)
#' glmCorrectLink <- glm(Y ~ X1, data = simdata, family = gaussian(link = "inverse"))
#' glmWrongLink <- glm(Y ~ X1, data = simdata, family = gaussian(link = "identity"))
#' summary(glmCorrectLink)$aic
#' summary(glmWrongLink)$aic
#' rm(simdata, glmCorrectLink, glmWrongLink)
#'
#'
#' # Does a forward stepwise search find the correct model for logistic regression? Yes
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
  defaultDispersion = 1
)

#' @rdname simulate_gaussian
#' @export
simulate_binomial <- make_simulating_function(
  validLinks = c("logit", "probit", "cauchit", "log", "cloglog"),
  defaultLink = "logit",
  defaultWeights = c(.1, .2),
  make_response = create_binomial,
  defaultDispersion = NULL
)

#' @rdname simulate_gaussian
#' @export
simulate_gamma <- make_simulating_function(
  validLinks = c("inverse", "identity", "log"),
  defaultLink = "inverse",
  defaultWeights = 1:3,
  make_response = create_gamma,
  defaultDispersion = 1
)

#' @rdname simulate_gaussian
#' @export
simulate_poisson <- make_simulating_function(
  validLinks = c("log", "identity", "sqrt"),
  defaultLink = "log",
  defaultWeights = 1:3,
  make_response = create_poisson,
  defaultDispersion = NULL
)

#' @rdname simulate_gaussian
#' @export
simulate_inverse_gaussion <- make_simulating_function(
  validLinks = c("1/mu^2", "inverse", "identity", "log"),
  defaultLink = "1/mu^2",
  defaultWeights = 1:3,
  make_response = create_inverse_gaussion,
  defaultDispersion = 1
)
