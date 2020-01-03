#' @keywords internal
# Helper function to make X
create_predictor <- function(weight, n) {
  return(stats::runif(n, 1, 2))
}

#' @keywords internal
# Functions to receive mu and return response variable for glm distributions
create_gaussian <- function(mu, n, ancillary) {
  return(matrix(stats::rnorm(n, mu, ancillary), ncol = 1))
}

#' @keywords internal
create_binomial <- function(mu, n, unused) {
  assertthat::assert_that(all(mu >= 0),
    msg = "Invalid weight and link combination. Choose a different link or weights."
  )
  assertthat::assert_that(all(mu <= 1),
    msg = "Invalid weight and link combination. Choose a different link or weights"
  )

  return(matrix(stats::rbinom(n, 1, mu), ncol = 1))
}

#' @keywords internal
create_gamma <- function(mu, n, ancillary) {
  assertthat::assert_that(all(mu > 0),
    msg = "Invalid weight and link combination. Choose a different link or weights."
  )

  return(matrix(stats::rgamma(n, mu / ancillary, scale = ancillary), ncol = 1))
}

#' @keywords internal
create_poisson <- function(mu, n, unused) {
  assertthat::assert_that(all(mu >= 0),
    msg = "Invalid weight and link combination. Choose a different link or weights."
  )

  return(matrix(stats::rpois(n, mu), ncol = 1))
}

#' @keywords internal
create_inverse_gaussian <- function(mu, n, ancillary) {
  assertthat::assert_that(all(mu > 0),
    msg = "Invalid weight and link combination. Choose a different link or weights."
  )

  return(matrix(statmod::rinvgauss(n = n, mean = mu, dispersion = ancillary), ncol = 1))
}

#' @keywords internal
create_negative_binomial <- function(mu, n, ancillary) {
  assertthat::assert_that(all(mu > 0),
    msg = "Invalid weight and link combination. Choose a different link or weights."
  )

  return(matrix(MASS::rnegbin(n = n, mu = mu, theta = ancillary), ncol = 1))
}

#' @keywords internal
create_tweedie <- function(mu, n, ancillary) {
  assertthat::assert_that(all(mu > 0),
    msg = "Invalid weight and link combination. Choose a different link or weights."
  )
  assertthat::assert_that(ancillary >= 1,
    msg = "Invalid ancillary. Should be in [1,2]."
  )
  assertthat::assert_that(ancillary <= 2,
    msg = "Invalid ancillary. Should be in [1,2]."
  )

  matrix(tweedie::rtweedie(n = n, mu = as.vector(mu), xi = ancillary, phi = 1), ncol = 1, nrow = n)
}

#' @keywords internal
#' A function factory
# Function to return a function that makes data perfect for glm model.
make_simulating_function <- function(validLinks, defaultLink, defaultWeights, make_response, defaultAncillary) {
  f <- function(N = 10000, link = defaultLink, weights = defaultWeights,
                unrelated = 0, ancillary = defaultAncillary) {

    ####################
    # Check inputs
    ####################
    assertthat::assert_that(assertthat::is.count(N))

    assertthat::assert_that(assertthat::is.string(link))
    assertthat::assert_that(link %in% validLinks,
      msg = "Argument link was not a valid link. See help(family) for valid links."
    )

    assertthat::assert_that(is.numeric(weights))
    assertthat::assert_that(length(weights) > 0)

    assertthat::assert_that(length(unrelated) == 1,
      msg = "Argument unrelated must have length 1."
    )
    assertthat::assert_that(unrelated >= 0)

    assertthat::assert_that(assertthat::is.scalar(ancillary) || is.null(ancillary),
      msg = "Argument ancillary must be a scalar or NULL."
    )
    assertthat::assert_that(ancillary > 0 || is.null(ancillary),
      msg = "Argument ancillary must be greater than 0 or NULL."
    )

    ####################
    # Create inverse link function
    ####################
    # gaussian
    if (link == "identity") {
      inv_link <- function(eta) {
        return(eta)
      }
    } else if (link == "log") {
      inv_link <- function(eta) {
        return(exp(eta))
      }
    } else if (link == "inverse") {
      inv_link <- function(eta) {
        return(1 / eta)
      }

      # binomial
      # Log is in gaussian section
    } else if (link == "logit") {
      inv_link <- function(eta) {
        return(exp(eta) / (1 + exp(eta)))
      }
    } else if (link == "probit") {
      inv_link <- function(eta) {
        return(stats::pnorm(eta))
      }
    } else if (link == "cauchit") {
      inv_link <- function(eta) {
        return(stats::pcauchy(eta))
      }
    } else if (link == "cloglog") {
      inv_link <- function(eta) {
        return(1 - exp(-exp(eta)))
      }

      # gamma
      # inverse, identity, and log are in gaussian section

      # poisson
      # log, identity are in gaussian
    } else if (link == "sqrt") {
      inv_link <- function(eta) {
        return(eta^2)
      }

      # inverse.gaussian
      # identity, inverse, and log are in gaussian
    } else if (link == "1/mu^2") {
      inv_link <- function(eta) {
        return(eta^-.5)
      }
    }

    # negative_binomial
    # log, identity, and sqrt are in gaussian and poisson



    ####################
    # Create predictors
    ####################
    X <- purrr::map_dfc(weights, create_predictor, n = N) %>%
      as.matrix()
    colnames(X) <- stringr::str_c(rep("X", length(weights)), 1:length(weights))

    if (unrelated > 0) {
      useless <- purrr::map_dfc(1:unrelated, create_predictor, n = N) %>%
        as.matrix()
      colnames(useless) <- stringr::str_c(rep("Unrelated", length(unrelated)), 1:unrelated)
    }

    ####################
    # Create response
    ####################
    B <- matrix(weights, ncol = 1)
    Mu <- inv_link(X %*% B + max(B))
    Y <- make_response(Mu, N, ancillary)
    colnames(Y) <- "Y"

    ####################
    # Convert to tibble
    ####################
    X <- dplyr::as_tibble(X)
    Y <- dplyr::as_tibble(Y)
    data <- dplyr::bind_cols(Y, X)

    # Add unrelated variables if requested.
    if (exists("useless")) {
      useless <- dplyr::as_tibble(useless)
      data <- dplyr::bind_cols(data, useless)
    }

    return(data)
  }
}
