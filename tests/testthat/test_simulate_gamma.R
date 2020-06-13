context("simulate_gamma")
library(GlmSimulatoR)
set.seed(1)

###############################################
# Run code
###############################################
default <- simulate_gamma()
test_that("Run default. Check structure.", {
  expect_true(all(class(default) == c("tbl_df", "tbl", "data.frame")))
  expect_true(nrow(default) == 10000)
  expect_true(all(colnames(default) == c("Y", "X1", "X2", "X3")))
  expect_true(min(default$X1) >= 1)
  expect_true(max(default$X1) <= 2)
  expect_true(min(default$X2) >= 1)
  expect_true(max(default$X2) <= 2)
  expect_true(min(default$X3) >= 1)
  expect_true(max(default$X3) <= 2)
})
rm(default)

test_that("Returns the correct number of rows.", {
  expect_equal(nrow(simulate_gamma(N = 10)), 10)
  expect_equal(nrow(simulate_gamma(N = 100)), 100)
  expect_equal(nrow(simulate_gamma(N = 1000)), 1000)
  expect_equal(nrow(simulate_gamma(N = 10000)), 10000)
})

test_that("Returns the correct number of predictors.", {
  expect_equal(ncol(simulate_gamma(weights = 1)), 2)
  expect_equal(ncol(simulate_gamma(weights = 1:2)), 3)
  expect_equal(ncol(simulate_gamma(weights = 1:3)), 4)
  expect_equal(ncol(simulate_gamma(weights = 1:4)), 5)
})

test_that("Returns the correct range for x.", {
  expect_true(max(simulate_gamma(weights = 1, xrange = 0)[,2]) <= 1)
  expect_true(min(simulate_gamma(weights = 1, xrange = 0)[,2]) >= 1)
  expect_true(max(simulate_gamma(weights = 1, xrange = 2)[,2]) <= 3)
  expect_true(min(simulate_gamma(weights = 1, xrange = 2)[,2]) >= 1)
  expect_true(max(simulate_gamma(weights = 1, xrange = 3)[,2]) <= 4)
  expect_true(min(simulate_gamma(weights = 1, xrange = 3)[,2]) >= 1)
  expect_true(max(simulate_gamma(weights = c(1,2), xrange = 0)[,3]) <= 1)
  expect_true(min(simulate_gamma(weights = c(1,2), xrange = 0)[,3]) >= 1)
  expect_true(max(simulate_gamma(weights = c(1,2), xrange = 2)[,3]) <= 3)
  expect_true(min(simulate_gamma(weights = c(1,2), xrange = 2)[,3]) >= 1)
  expect_true(max(simulate_gamma(weights = c(1,2), xrange = 3)[,3]) <= 4)
  expect_true(min(simulate_gamma(weights = c(1,2), xrange = 3)[,3]) >= 1)
})

test_that("Returns the correct number of unrelated variables.", {
  expect_equal(ncol(simulate_gamma(weights = 1, unrelated = 0)), 2)
  expect_equal(ncol(simulate_gamma(weights = 1, unrelated = 1)), 3)
  expect_equal(ncol(simulate_gamma(weights = 1, unrelated = 2)), 4)
  expect_equal(ncol(simulate_gamma(weights = 1, unrelated = 3)), 5)

  expect_equal(ncol(simulate_gamma(weights = 1:2, unrelated = 0)), 3)
  expect_equal(ncol(simulate_gamma(weights = 1:2, unrelated = 1)), 4)
  expect_equal(ncol(simulate_gamma(weights = 1:2, unrelated = 2)), 5)
  expect_equal(ncol(simulate_gamma(weights = 1:2, unrelated = 3)), 6)
})

test_that("All links execute", {
  expect_true(all(class(simulate_gamma(link = "inverse")) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_gamma(link = "identity")) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_gamma(link = "log")) == c("tbl_df", "tbl", "data.frame")))
})

test_that("Ancillary parameter works as expected", {
  expect_true(simulate_gamma(ancillary = .25)$Y %>% sd() < simulate_gamma(ancillary = 1)$Y %>% sd())
})

###############################################
# Input checking
###############################################
test_that("Confirm input checing works.", {
  expect_error(simulate_gamma(N = -1), NULL)
  expect_error(simulate_gamma(N = c(100, 200)), NULL)
  expect_error(simulate_gamma(link = "sqrt"), NULL)
  expect_error(simulate_gamma(weights = c()), NULL)
  expect_error(simulate_gamma(xrange = "asdf"), NULL)
  expect_error(simulate_gamma(xrange = c()), NULL)
  expect_error(simulate_gamma(xrange = c(1,2)), NULL)
  expect_error(simulate_gamma(xrange = -1), NULL)
  expect_error(simulate_gamma(unrelated = -1), NULL)
  expect_error(simulate_gamma(unrelated = c(10, 20)), NULL)
  expect_error(simulate_gamma(ancillary = -1), NULL)
  expect_error(simulate_gamma(ancillary = c(10, 20)), NULL)
})
