library(GlmSimulatoR)
set.seed(1)

###############################################
# Test make_simulating_function
###############################################
custom_identity <- function(mu, n, ancillary) {
  return(mu)
}

temp <- GlmSimulatoR:::make_simulating_function(
  valid_links = c("identity", "log", "inverse"),
  default_link = "identity",
  default_weights = 1:3,
  default_range = 1,
  make_response = custom_identity,
  default_ancillary = 1
)

test_that("Confirm function has correct structure", {
  expect_true(class(temp) == "function")
  expect_true(all(names(formals(temp)) ==
    c(
      "N", "link", "weights", "x_range", "unrelated",
      "ancillary"
    )))
})
rm(custom_identity)

custom_identity <- function(mu, n, unused) {
  return(mu)
}

temp <- GlmSimulatoR:::make_simulating_function(
  valid_links = c("identity", "log", "inverse"),
  default_link = "identity",
  default_weights = 1:3,
  default_range = 1,
  make_response = custom_identity,
  default_ancillary = NULL
)

test_that("Confirm function has correct structure", {
  expect_true(class(temp) == "function")
  expect_true(all(names(formals(temp)) ==
    c("N", "link", "weights", "x_range", "unrelated")))
})
rm(custom_identity)

###############################################
# input checking
###############################################
test_that("Check valid_link and default_link", {
  expect_error(
    GlmSimulatoR:::make_simulating_function("foo", "bar", 1, 1, 1, 1),
    "Argument default_link was not a valid_links."
  )
})

test_that("Check default_weights", {
  expect_error(
    GlmSimulatoR:::make_simulating_function(
      "identity", "identity", "foo",
      1, 1, 1
    ), NULL
  )
  expect_error(
    GlmSimulatoR:::make_simulating_function(
      "identity", "identity",
      vector(mode = "numeric", length = 0),
      1, 1, 1
    ), NULL
  )
})

test_that("Check default_weights", {
  expect_error(GlmSimulatoR:::make_simulating_function(
    "identity", "identity",
    1, "foo", 1, 1
  ), NULL)
  expect_error(GlmSimulatoR:::make_simulating_function(
    "identity", "identity",
    1, -1, 1, 1
  ), NULL)
})

temp_01 <- function(foo, n, ancillary) {}
temp_02 <- function(mu, foo, ancillary) {}
temp_03 <- function(mu, n, foo) {}
test_that("Check make_response", {
  expect_error(GlmSimulatoR:::make_simulating_function(
    "identity", "identity",
    1, 1, "foo", 1
  ), NULL)
  expect_error(GlmSimulatoR:::make_simulating_function(
    "identity", "identity",
    1, 1, temp_01, 1
  ), NULL)
  expect_error(GlmSimulatoR:::make_simulating_function(
    "identity", "identity",
    1, 1, temp_02, 1
  ), NULL)
  expect_error(GlmSimulatoR:::make_simulating_function(
    "identity", "identity",
    1, 1, temp_03, 1
  ), NULL)
})
rm(temp_01, temp_02, temp_03)

test_that("Check default_ancillary", {
  expect_error(GlmSimulatoR:::make_simulating_function(
    "identity", "identity",
    1, 1, custom_identity,
    "foo"
  ), NULL)
  expect_error(GlmSimulatoR:::make_simulating_function(
    "identity", "identity",
    1, 1, custom_identity,
    -1
  ), NULL)
})

###############################################
# Test random data creators
###############################################

test_that("Confirm functions return a matrix with correct dimensions", {
  expect_true(all(dim(GlmSimulatoR:::create_gaussian(
    matrix(c(1, 2, 3), ncol = 1), 3, 1
  )) == c(3, 1)))
  expect_true(all(dim(GlmSimulatoR:::create_binomial(
    matrix(c(.1, .2, .3), ncol = 1), 3, 1
  )) == c(3, 1)))
  expect_true(all(dim(GlmSimulatoR:::create_gamma(
    matrix(c(1, 2, 3), ncol = 1), 3, 1
  )) == c(3, 1)))
  expect_true(all(dim(GlmSimulatoR:::create_poisson(
    matrix(c(1, 2, 3), ncol = 1), 3, 1
  )) == c(3, 1)))
  expect_true(all(dim(GlmSimulatoR:::create_inverse_gaussian(
    matrix(c(1, 2, 3), ncol = 1), 3, 1
  )) == c(3, 1)))
  expect_true(all(dim(GlmSimulatoR:::create_negative_binomial(
    matrix(c(1, 2, 3), ncol = 1), 3, 1
  )) == c(3, 1)))
  expect_true(all(dim(GlmSimulatoR:::create_tweedie(
    matrix(c(1, 2, 3), ncol = 1), 3, 1
  )) == c(3, 1)))
})

test_that("Confirm gaussia input checking works", {
  expect_error(GlmSimulatoR:::create_gaussian(
    matrix(c(-.1, .2, .3), ncol = 1), 3, -1
  ))
})

test_that("Confirm binomial input checking works", {
  expect_error(GlmSimulatoR:::create_binomial(
    matrix(c(-.1, .2, .3), ncol = 1), 3, 1
  ))
  expect_error(GlmSimulatoR:::create_binomial(
    matrix(c(1.1, .2, .3), ncol = 1), 3, 1
  ))
})

test_that("Confirm gamma input checking works", {
  expect_error(GlmSimulatoR:::create_gamma(matrix(
    c(-.1, .2, .3),
    ncol = 1
  ), 3, 1))
})

test_that("Confirm poisson input checking works", {
  expect_error(GlmSimulatoR:::create_poisson(matrix(
    c(-.1, .2, .3),
    ncol = 1
  ), 3, 1))
})

test_that("Confirm inverse gaussian input checking works", {
  expect_error(GlmSimulatoR:::create_inverse_gaussian(matrix(
    c(-.1, .2, .3),
    ncol = 1
  ), 3, 1))
})

test_that("Confirm negative binomial input checking works", {
  expect_error(GlmSimulatoR:::create_negative_binomial(matrix(
    c(-.1, .2, .3),
    ncol = 1
  ), 3, 1))
})

test_that("Confirm tweedie input checking works", {
  expect_error(GlmSimulatoR:::create_tweedie(
    matrix(c(-.1, .2, .3), ncol = 1), 3, 1
  ))

  expect_error(GlmSimulatoR:::create_tweedie(
    matrix(c(.1, .2, .3), ncol = 1), 3, .5
  ))
  expect_error(GlmSimulatoR:::create_tweedie(
    matrix(c(.1, .2, .3), ncol = 1), 3, 2.5
  ))
})
