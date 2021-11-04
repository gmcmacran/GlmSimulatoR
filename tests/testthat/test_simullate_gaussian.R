library(GlmSimulatoR)
set.seed(1)

###############################################
# Run code
###############################################
default <- simulate_gaussian()

model <- glm(formula = Y ~ X1 + X2 + X3, data = default, family = gaussian())
params <- c(1, 2, 3)
params <- c(max(params), params)

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
  expect_true(all(max(abs(model$coefficients - params)) <= .1))
})
rm(default, model, params)

test_that("Returns the correct number of rows.", {
  expect_equal(nrow(simulate_gaussian(N = 10)), 10)
  expect_equal(nrow(simulate_gaussian(N = 100)), 100)
  expect_equal(nrow(simulate_gaussian(N = 1000)), 1000)
  expect_equal(nrow(simulate_gaussian(N = 10000)), 10000)
})

test_that("Returns the correct number of predictors.", {
  expect_equal(ncol(simulate_gaussian(weights = 1)), 2)
  expect_equal(ncol(simulate_gaussian(weights = 1:2)), 3)
  expect_equal(ncol(simulate_gaussian(weights = 1:3)), 4)
  expect_equal(ncol(simulate_gaussian(weights = 1:4)), 5)
})

test_that("Returns the correct range for x.", {
  expect_true(max(simulate_gaussian(weights = 1, xrange = 0)[, 2]) <= 1)
  expect_true(min(simulate_gaussian(weights = 1, xrange = 0)[, 2]) >= 1)
  expect_true(max(simulate_gaussian(weights = 1, xrange = 2)[, 2]) <= 3)
  expect_true(min(simulate_gaussian(weights = 1, xrange = 2)[, 2]) >= 1)
  expect_true(max(simulate_gaussian(weights = 1, xrange = 3)[, 2]) <= 4)
  expect_true(min(simulate_gaussian(weights = 1, xrange = 3)[, 2]) >= 1)
  expect_true(max(simulate_gaussian(weights = c(1, 2), xrange = 0)[, 3]) <= 1)
  expect_true(min(simulate_gaussian(weights = c(1, 2), xrange = 0)[, 3]) >= 1)
  expect_true(max(simulate_gaussian(weights = c(1, 2), xrange = 2)[, 3]) <= 3)
  expect_true(min(simulate_gaussian(weights = c(1, 2), xrange = 2)[, 3]) >= 1)
  expect_true(max(simulate_gaussian(weights = c(1, 2), xrange = 3)[, 3]) <= 4)
  expect_true(min(simulate_gaussian(weights = c(1, 2), xrange = 3)[, 3]) >= 1)
})

test_that("Returns the correct number of unrelated variables.", {
  expect_equal(ncol(simulate_gaussian(weights = 1, unrelated = 0)), 2)
  expect_equal(ncol(simulate_gaussian(weights = 1, unrelated = 1)), 3)
  expect_equal(ncol(simulate_gaussian(weights = 1, unrelated = 2)), 4)
  expect_equal(ncol(simulate_gaussian(weights = 1, unrelated = 3)), 5)

  expect_equal(ncol(simulate_gaussian(weights = 1:2, unrelated = 0)), 3)
  expect_equal(ncol(simulate_gaussian(weights = 1:2, unrelated = 1)), 4)
  expect_equal(ncol(simulate_gaussian(weights = 1:2, unrelated = 2)), 5)
  expect_equal(ncol(simulate_gaussian(weights = 1:2, unrelated = 3)), 6)
})

test_that("All links execute", {
  expect_true(all(class(simulate_gaussian(link = "identity")) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_gaussian(link = "log")) == c("tbl_df", "tbl", "data.frame")))
  expect_true(all(class(simulate_gaussian(link = "inverse")) == c("tbl_df", "tbl", "data.frame")))
})

test_that("Ancillary parameter works as expected", {
  expect_true(simulate_gaussian()$Y %>% sd() < simulate_gaussian(ancillary = 5)$Y %>% sd())
})

###############################################
# Input checking
###############################################
test_that("Confirm input checing works.", {
  expect_error(simulate_gaussian(N = -1), NULL)
  expect_error(simulate_gaussian(N = c(100, 200)), NULL)
  expect_error(simulate_gaussian(link = "sqrt"), NULL)
  expect_error(simulate_gaussian(weights = c()), NULL)
  expect_error(simulate_gaussian(xrange = "asdf"), NULL)
  expect_error(simulate_gaussian(xrange = c()), NULL)
  expect_error(simulate_gaussian(xrange = c(1, 2)), NULL)
  expect_error(simulate_gaussian(xrange = -1), NULL)
  expect_error(simulate_gaussian(unrelated = -1), NULL)
  expect_error(simulate_gaussian(unrelated = c(10, 20)), NULL)
  expect_error(simulate_gaussian(ancillary = -1), NULL)
  expect_error(simulate_gaussian(ancillary = c(10, 20)), NULL)
})
