context("make_simulating_function")
library(GlmSimulatoR)

###############################################
# Run code
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
