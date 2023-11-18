#' Create ideal data for a generalized linear model.
#'
#' @param N Sample size. (Default: 10000)
#' @param link Link function. See \code{\link[stats]{family}} for details.
#' @param weights Betas in glm model.
#' @param x_range range of x variables.
#' @param unrelated Number of unrelated features to return. (Default: 0)
#' @param ancillary Ancillary parameter for continuous families and negative
#' binomial. See details.
#' @return A tibble with a response variable and predictors.
#' @details
#'
#' For many families, it is possible to pick weights that cause inverse
#' link(X * weights) to be mathematically invalid. For example, the log link
#' for binomial regression defines P(Y=1) as exp(X * weights) which can be above
#' one. If this happens, the function will error with a helpful message.
#'
#' The intercept in the underlying link(Y) = X * weights + intercept is always
#' max(weights). In simulate_gaussian(link = "inverse", weights = 1:3), the
#' model is (1/Y) = 1*X1 + 2*X2 + 3*X3 + 3.
#'
#'
#'  links
#'  \itemize{
#'   \item gaussian: identity, log, inverse
#'   \item binomial: logit, probit, cauchit, loglog, cloglog, log, logc,
#'   identity
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
#' linear_model <- lm(Y ~ X1 + X2 + X3, data = simdata)
#' glm_model <- glm(Y ~ X1 + X2 + X3,
#'   data = simdata,
#'   family = gaussian(link = "identity")
#' )
#' summary(linear_model)
#' summary(glm_model)
#' rm(linear_model, glm_model, simdata)
#'
#' # If the link is not identity, will the response
#' # variable still be normal? Yes
#' set.seed(1)
#' simdata <- simulate_gaussian(N = 1000, link = "log", weights = c(.1, .2))
#'
#' ggplot(simdata, aes(x = Y)) +
#'   geom_histogram(bins = 30)
#' rm(simdata)
#'
#' # Is AIC lower for the correct link? For ten thousand data points, depends
#' # on seed!
#' set.seed(1)
#' simdata <- simulate_gaussian(N = 10000, link = "inverse", weights = 1)
#' glm_correct_link <- glm(Y ~ X1,
#'   data = simdata,
#'   family = gaussian(link = "inverse")
#' )
#' glm_wrong_link <- glm(Y ~ X1,
#'   data = simdata,
#'   family = gaussian(link = "identity")
#' )
#' summary(glm_correct_link)$aic
#' summary(glm_wrong_link)$aic
#' rm(simdata, glm_correct_link, glm_wrong_link)
#'
#'
#' # Does a stepwise search find the correct model for logistic regression? Yes
#' # 3 related variables. 3 unrelated variables.
#' set.seed(1)
#' simdata <- simulate_binomial(
#'   N = 10000, link = "logit",
#'   weights = c(.3, .4, .5), unrelated = 3
#' )
#'
#' scope_arg <- list(
#'   lower = Y ~ 1,
#'   upper = Y ~ X1 + X2 + X3 + Unrelated1 + Unrelated2 + Unrelated3
#' )
#'
#' starting_model <- glm(Y ~ 1,
#'   data = simdata,
#'   family = binomial(link = "logit")
#' )
#' glm_model <- stepAIC(starting_model, scope_arg)
#' summary(glm_model)
#' rm(simdata, scope_arg, starting_model, glm_model)
#'
#' # When the resposne is a gamma distribution, what does a scatter plot between
#' # X and Y look like?
#' set.seed(1)
#' simdata <- simulate_gamma(weights = 1)
#' ggplot(simdata, aes(x = X1, y = Y)) +
#'   geom_point()
#' rm(simdata)
#' @export
simulate_gaussian <- make_simulating_function(
  valid_links = c("identity", "log", "inverse"),
  default_link = "identity",
  default_weights = 1:3,
  default_range = 1,
  make_response = create_gaussian,
  default_ancillary = 1
)

#' @rdname simulate_gaussian
#' @export
simulate_binomial <- make_simulating_function(
  valid_links = c(
    "logit", "probit", "cauchit", "log", "cloglog", "loglog",
    "logc", "identity"
  ),
  default_link = "logit",
  default_weights = c(.1, .2),
  default_range = 1,
  make_response = GlmSimulatoR:::create_binomial,
  default_ancillary = NULL
)

#' @rdname simulate_gaussian
#' @export
simulate_gamma <- make_simulating_function(
  valid_links = c("inverse", "identity", "log"),
  default_link = "inverse",
  default_weights = 1:3,
  default_range = 1,
  make_response = GlmSimulatoR:::create_gamma,
  default_ancillary = .05
)

#' @rdname simulate_gaussian
#' @export
simulate_poisson <- make_simulating_function(
  valid_links = c("log", "identity", "sqrt"),
  default_link = "log",
  default_weights = c(.5, 1),
  default_range = 1,
  make_response = GlmSimulatoR:::create_poisson,
  default_ancillary = NULL
)

#' @rdname simulate_gaussian
#' @export
simulate_inverse_gaussian <- make_simulating_function(
  valid_links = c("1/mu^2", "inverse", "identity", "log"),
  default_link = "1/mu^2",
  default_weights = 1:3,
  default_range = 1,
  make_response = GlmSimulatoR:::create_inverse_gaussian,
  default_ancillary = .3333
)

#' @rdname simulate_gaussian
#' @export
simulate_negative_binomial <- make_simulating_function(
  valid_links = c("log", "identity", "sqrt"),
  default_link = "log",
  default_weights = c(.5, 1),
  default_range = 1,
  make_response = GlmSimulatoR:::create_negative_binomial,
  default_ancillary = 1
)

#' @rdname simulate_gaussian
#' @export
simulate_tweedie <- make_simulating_function(
  valid_links = c("log", "identity", "sqrt", "inverse"),
  default_link = "log",
  default_weights = c(.02),
  default_range = 1,
  make_response = GlmSimulatoR:::create_tweedie,
  default_ancillary = 1.15
)
