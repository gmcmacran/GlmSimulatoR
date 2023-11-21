#' @keywords internal
# Functions to receive mu and return response variable for glm distributions
create_gaussian <- function(mu, n, ancillary) {
  assertthat::assert_that(ancillary > 0,
    msg = "Invalid ancillary. Should be positive."
  )

  return(matrix(stats::rnorm(n, mu, ancillary), ncol = 1))
}

#' @keywords internal
create_binomial <- function(mu, n, unused) {
  assertthat::assert_that(all(mu >= 0),
    msg = "Invalid weight and link combination. Choose a different link or
    weights."
  )
  assertthat::assert_that(all(mu <= 1),
    msg = "Invalid weight and link combination. Choose a different link or
    weights"
  )

  return(matrix(stats::rbinom(n, 1, mu), ncol = 1))
}

#' @keywords internal
create_gamma <- function(mu, n, ancillary) {
  assertthat::assert_that(all(mu > 0),
    msg = "Invalid weight and link combination. Choose a different link or
    weights."
  )

  return(matrix(stats::rgamma(n, mu / ancillary, scale = ancillary), ncol = 1))
}

#' @keywords internal
create_poisson <- function(mu, n, unused) {
  assertthat::assert_that(all(mu >= 0),
    msg = "Invalid weight and link combination. Choose a different link or
    weights."
  )

  return(matrix(stats::rpois(n, mu), ncol = 1))
}

#' @keywords internal
create_inverse_gaussian <- function(mu, n, ancillary) {
  assertthat::assert_that(all(mu > 0),
    msg = "Invalid weight and link combination. Choose a different link or
    weights."
  )

  return(matrix(
    statmod::rinvgauss(n = n, mean = mu, dispersion = ancillary),
    ncol = 1
  ))
}

#' @keywords internal
create_negative_binomial <- function(mu, n, ancillary) {
  assertthat::assert_that(all(mu > 0),
    msg = "Invalid weight and link combination. Choose a different link or
    weights."
  )

  return(matrix(MASS::rnegbin(n = n, mu = mu, theta = ancillary), ncol = 1))
}

#' @keywords internal
create_tweedie <- function(mu, n, ancillary) {
  assertthat::assert_that(all(mu > 0),
    msg = "Invalid weight and link combination. Choose a different link or
    weights."
  )
  assertthat::assert_that(ancillary >= 1,
    msg = "Invalid ancillary. Should be in [1,2]."
  )
  assertthat::assert_that(ancillary <= 2,
    msg = "Invalid ancillary. Should be in [1,2]."
  )

  return(matrix(
    tweedie::rtweedie(n = n, mu = as.vector(mu), xi = ancillary, phi = 1),
    ncol = 1, nrow = n
  ))
}

#' @keywords internal
#' A function factory
# Function to return a function that makes data perfect for glm model.
make_simulating_function <- function(valid_links, default_link, default_weights,
                                     default_range, make_response,
                                     default_ancillary) {
  force(valid_links)
  force(default_link)
  force(default_weights)
  force(default_range)
  force(make_response)
  force(default_ancillary)

  assertthat::assert_that(assertthat::is.string(default_link))
  assertthat::assert_that(default_link %in% valid_links,
    msg = "Argument default_link was not a valid_links."
  )

  assertthat::assert_that(is.numeric(default_weights))
  assertthat::assert_that(length(default_weights) > 0)

  assertthat::assert_that(assertthat::is.number(default_range))
  assertthat::assert_that(default_range >= 0)

  assertthat::assert_that(is.function(make_response),
    msg = "Argument make_response must be a function."
  )
  args <- names(formals(make_response))
  assertthat::assert_that(args[1] == "mu",
    msg = "First argument to function must be mu."
  )
  assertthat::assert_that(args[2] == "n",
    msg = "Second argument to function must be n."
  )
  assertthat::assert_that(args[3] == "ancillary" || args[3] == "unused",
    msg = "Third argument to function must be ancillary
                          or unused."
  )

  assertthat::assert_that(
    (is.numeric(default_ancillary) &&
      assertthat::is.scalar(default_ancillary)) ||
      is.null(default_ancillary),
    msg = "Argument default_ancillary must be a numeric scalar or NULL."
  )
  assertthat::assert_that(default_ancillary > 0 || is.null(default_ancillary),
    msg = "Argument default_ancillary must be greater
                          than 0 or NULL."
  )

  # ancillary is only meaningful for some glm famalies
  # For poisson and binomial it is not needed from a math perspective
  # Code still accepts the parameter and just does nothing with it.
  # Want to hide the argument from user in this case.
  # So setting a dummy value and deleting argument from f_out.
  if (is.null(default_ancillary)) {
    ancillary <- 1
  }

  f_out <- function(N = 10000, link, weights, x_range, unrelated = 0,
                    ancillary) {
    ####################
    # Check inputs
    ####################
    assertthat::assert_that(assertthat::is.count(N))

    assertthat::assert_that(assertthat::is.string(link))
    assertthat::assert_that(link %in% valid_links,
      msg = "Argument link was not a valid link. See documentation for valid
      links."
    )

    assertthat::assert_that(is.numeric(weights))
    assertthat::assert_that(length(weights) > 0)

    assertthat::assert_that(assertthat::is.number(x_range))
    assertthat::assert_that(x_range >= 0)

    assertthat::assert_that(length(unrelated) == 1,
      msg = "Argument unrelated must have length 1."
    )
    assertthat::assert_that(unrelated >= 0)

    assertthat::assert_that(
      assertthat::is.scalar(ancillary) ||
        is.null(ancillary),
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
    } else if (link == "loglog") {
      inv_link <- function(eta) {
        return(exp(-exp(-eta)))
      }
    } else if (link == "logc") {
      inv_link <- function(eta) {
        return(1 - exp(eta))
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
    x <- matrix(stats::runif(N * length(weights), 1, x_range + 1),
      nrow = N,
      ncol = length(weights)
    )
    colnames(x) <- stringr::str_c(rep("X", length(weights)), seq_along(weights))

    if (unrelated > 0) {
      useless <- matrix(stats::runif(N * unrelated, 1, x_range + 1),
        nrow = N,
        ncol = unrelated
      )
      colnames(useless) <- stringr::str_c(
        rep("Unrelated", length(unrelated)),
        1:unrelated
      )
    }

    ####################
    # Create response
    ####################
    b <- matrix(weights, ncol = 1)
    mu <- inv_link(x %*% b + max(b))
    y <- make_response(mu, N, ancillary)
    colnames(y) <- "Y"

    ####################
    # Convert to tibble
    ####################
    x <- dplyr::as_tibble(x)
    y <- dplyr::as_tibble(y)
    data <- dplyr::bind_cols(y, x)

    # Add unrelated variables if requested.
    if (exists("useless")) {
      useless <- dplyr::as_tibble(useless)
      data <- dplyr::bind_cols(data, useless)
    }

    return(data)
  }

  # Set default values
  formals(f_out)$link <- default_link
  formals(f_out)$weights <- default_weights
  formals(f_out)$x_range <- default_range
  formals(f_out)$ancillary <- default_ancillary

  return(f_out)
}
