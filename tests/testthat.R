library(testthat)
library(rrrsa)

test_check("rrrsa")

#' unit tests
#'
test_that("informativity returns valid value", {
  expect_equal(informativity(0), 0)
  expect_equal(informativity(0.3), 0.3)
})

test_that("informativity passed correct value", {
  expect_error(informativity(-1.5),)
  expect_error(informativity(2),)
})
