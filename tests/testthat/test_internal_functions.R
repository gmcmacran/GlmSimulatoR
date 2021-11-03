context("make_simulating_function")
library(GlmSimulatoR)

###############################################
# Test make_simulating_function
###############################################
temp <- GlmSimulatoR:::make_simulating_function(
  validLinks = c("identity", "log", "inverse"),
  defaultLink = "identity",
  defaultWeights = 1:3,
  defaultRange = 1,
  make_response = identity,
  defaultAncillary = 1
)

test_that("Confirm function has correct structure", {
  expect_true(class(temp) == "function")
  expect_true(all(names(formals(temp)) == c("N", "link", "weights", "xrange", "unrelated", "ancillary")))
})

temp <- GlmSimulatoR:::make_simulating_function(
  validLinks = c("identity", "log", "inverse"),
  defaultLink = "identity",
  defaultWeights = 1:3,
  defaultRange = 1,
  make_response = identity,
  defaultAncillary = NULL
)

test_that("Confirm function has correct structure", {
  expect_true(class(temp) == "function")
  expect_true(all(names(formals(temp)) == c("N", "link", "weights", "xrange", "unrelated")))
})

###############################################
# Test random data creaters
###############################################

test_that("Confirm functions return a matrix with correct dimensions", {
  expect_true(all(dim(GlmSimulatoR:::create_gaussian(matrix(c(1, 2, 3), ncol = 1), 3, 1)) == c(3, 1)))
  expect_true(all(dim(GlmSimulatoR:::create_binomial(matrix(c(.1, .2, .3), ncol = 1), 3, 1)) == c(3, 1)))
  expect_true(all(dim(GlmSimulatoR:::create_gamma(matrix(c(1, 2, 3), ncol = 1), 3, 1)) == c(3, 1)))
  expect_true(all(dim(GlmSimulatoR:::create_poisson(matrix(c(1, 2, 3), ncol = 1), 3, 1)) == c(3, 1)))
  expect_true(all(dim(GlmSimulatoR:::create_inverse_gaussian(matrix(c(1, 2, 3), ncol = 1), 3, 1)) == c(3, 1)))
  expect_true(all(dim(GlmSimulatoR:::create_negative_binomial(matrix(c(1, 2, 3), ncol = 1), 3, 1)) == c(3, 1)))
  expect_true(all(dim(GlmSimulatoR:::create_tweedie(matrix(c(1, 2, 3), ncol = 1), 3, 1)) == c(3, 1)))
})

test_that("Confirm gaussia input checking works", {
  expect_error(GlmSimulatoR:::create_gaussian(matrix(c(-.1, .2, .3), ncol = 1), 3, -1))

  expect_error(GlmSimulatoR:::create_tweedie(matrix(c(.1, .2, .3), ncol = 1), 3, .5))
  expect_error(GlmSimulatoR:::create_tweedie(matrix(c(.1, .2, .3), ncol = 1), 3, 2.5))
})

test_that("Confirm binomial input checking works", {
  expect_error(GlmSimulatoR:::create_binomial(matrix(c(-.1, .2, .3), ncol = 1), 3, 1))
  expect_error(GlmSimulatoR:::create_binomial(matrix(c(1.1, .2, .3), ncol = 1), 3, 1))
})

test_that("Confirm gamma input checking works", {
  expect_error(GlmSimulatoR:::create_gamma(matrix(c(-.1, .2, .3), ncol = 1), 3, 1))
})

test_that("Confirm poisson input checking works", {
  expect_error(GlmSimulatoR:::create_poisson(matrix(c(-.1, .2, .3), ncol = 1), 3, 1))
})

test_that("Confirm inverse gaussian input checking works", {
  expect_error(GlmSimulatoR:::create_inverse_gaussian(matrix(c(-.1, .2, .3), ncol = 1), 3, 1))
})

test_that("Confirm negative binomial input checking works", {
  expect_error(GlmSimulatoR:::create_negative_binomial(matrix(c(-.1, .2, .3), ncol = 1), 3, 1))
})

test_that("Confirm tweedie input checking works", {
  expect_error(GlmSimulatoR:::create_tweedie(matrix(c(-.1, .2, .3), ncol = 1), 3, 1))

  expect_error(GlmSimulatoR:::create_tweedie(matrix(c(.1, .2, .3), ncol = 1), 3, .5))
  expect_error(GlmSimulatoR:::create_tweedie(matrix(c(.1, .2, .3), ncol = 1), 3, 2.5))
})
