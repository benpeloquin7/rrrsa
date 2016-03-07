library(testthat)
library(rrrsa)

test_check("rrrsa")


## rsa.informativity()
## -------------------
## Informativity returns correct value
test_that("informativity returns valid value", {
  expect_equal(rsa.informativity(0), 0)
  expect_equal(rsa.informativity(0.3), 0.3)
  expect_equal(rsa.informativity(0.0), 0.0)
  expect_equal(rsa.informativity(1.0), 1)
})
## Informativity passed correct values
test_that("informativity passed valid value", {

  expect_error(rsa.informativity(m_u = -1.5))    #! no negative vals
  expect_error(rsa.informativity(m_u = 2))       #! values must be between [0, 1]
  expect_error(rsa.informativity(m_u = 0.5, alpha = 0),
               "Invalid alpha, must be a positive, non-zero numerical expression")  #! alpha values must be (0, +inf)
  expect_error(rsa.informativity(m_u = 0.5, alpha = -1),
               "Invalid alpha, must be a positive, non-zero numerical expression")  #! alpha values must be (0, +inf)
})
debug(rsa.informativity)
undebug(rsa.informativity)

## rsa.utility()
## -------------
test_that("utilty returns valid value", {
  items1 <- c(0.5, 0.5)
  items2 <- c(0.2, 0.8)
  costs <- c((-1 * (-log(0.5) + log(0.2))), (-1 * (-log(0.5) + log(0.8))))
  expect_equal(rsa.utility(items = items1), items1)
  expect_equal(rsa.utility(items = items2), items2)
  expect_equal(rsa.utility(items = items2, costs = costs), items1)
})

test_that("utility passed valid value", {
  items1 <- c("apple", "banana", "pear")
  costs1 <- seq(1,5)
  costs2 <- seq(1,length(items1))
  expect_error(rsa.utility(items = items1, costs = costs1), #! items and costs vectors must be same length
               "Item and cost dimensions do not match")
  expect_error(rsa.utility(items = items1, costs = costs2, alpha = costs1), #! alpha should be length 1
               "Alpha should be a single numerical expression")
})
debug(rsa.utility)
undebug(rsa.utility)
