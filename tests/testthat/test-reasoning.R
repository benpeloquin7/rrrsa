context("reasoning")

## rsa.informativity()
## -------------------
test_that("rsa.informativity returns valid values", {
  expect_equal(rsa.informativity(0), 0)
  expect_equal(rsa.informativity(0.3), 0.3)
  expect_equal(rsa.informativity(0.0), 0.0)
  expect_equal(rsa.informativity(1.0), 1)
})
test_that("rsa.informativity passed valid values", {

  expect_error(rsa.informativity(m_u = -1.5))    #! no negative vals
  expect_error(rsa.informativity(m_u = 2))       #! values must be between [0, 1]
  expect_error(rsa.informativity(m_u = 0.5, alpha = 0),
               "Invalid alpha, must be a positive, non-zero numerical expression")  #! alpha values must be (0, +inf)
  expect_error(rsa.informativity(m_u = 0.5, alpha = -1),
               "Invalid alpha, must be a positive, non-zero numerical expression")  #! alpha values must be (0, +inf)
})

## rsa.utility()
## -------------
test_that("rsa.utilty returns valid values", {
  items1 <- c(0.5, 0.5)
  items2 <- c(0.2, 0.8)
  costs <- c((-1 * (-log(0.5) + log(0.2))), (-1 * (-log(0.5) + log(0.8))))
  expect_equal(rsa.utility(items = items1), items1)
  expect_equal(rsa.utility(items = items2), items2)
  expect_equal(rsa.utility(items = items2, costs = costs), items1)
})
test_that("rsa.utility passed valid values", {
  items1 <- c("apple", "banana", "pear")
  costs1 <- seq(1,5)
  costs2 <- seq(1,length(items1))
  expect_error(rsa.utility(items = items1, costs = costs1), #! items and costs vectors must be same length
               "Item and cost dimensions do not match")
  expect_error(rsa.utility(items = items1, costs = costs2, alpha = costs1), #! alpha should be length 1
               "Invalid alpha amount, must be numerical expression strictly greather than 0")
})

## rsa.fullRecursion()
## -------------------
test_that("rsa.fullRecursion returns valid values", {
  m <- matrix(data = c(1.0, 0.0, 0.0, 0.0, 0.0,
                       0.0, 0.25, 0.25, 0.25, 0.25,
                       0.0, 0.0, 0.0, 0.0, 1.0), nrow = 5)
  runMatrix <- rsa.fullRecursion(m)
  answer_m <- matrix(data = c(1.0, 0.0, 0.0, 0.0, 0.0,
                              0.0, 0.3125, 0.3125, 0.3125, 0.0625,
                              0.0, 0.0, 0.0, 0.0, 1.0), nrow = 5)

  expect_that(rsa.fullRecursion(m), is_equivalent_to(answer_m)) #! basic inference computation
  expect_that(sum(runMatrix), equals(ncol(runMatrix)))          #! normalized values
})
test_that("rsa.fullRecursion passed valid values", {
  m <- matrix(data = c(1.0, 0.0, 0.0, 0.0, 0.0,
                       0.0, 0.25, 0.25, 0.25, 0.25,
                       0.0, 0.0, 0.0, 0.0, 1.0), nrow = 5)
  fakeM <- data.frame(m)
  badCosts1 <- c(1:(ncol(m)-1))
  badPriors1 <- c(1:(nrow(m)+1))
  expect_error(rsa.fullRecursion(fakeM), "'m' should be a matrix")
  expect_error(rsa.fullRecursion(m, costs = badCosts1), "Incorrect cost vector dimensions")
  expect_error(rsa.fullRecursion(m, priors = badPriors1), "Incorrect priors vector dimensions")
  expect_error(rsa.fullRecursion(m, alpha = -1), "Invalid alpha amount, must be numerical expression strictly greather than 0")
  expect_error(rsa.fullRecursion(m, alpha = c(1, 2)), "Invalid alpha amount, must be numerical expression strictly greather than 0")
  expect_error(rsa.fullRecursion(m, alpha = "apple"), "Invalid alpha amount, must be numerical expression strictly greather than 0")
})

## rsa.reason()
## ------------
test_that("rsa.reason returns valid values", {
  m <- matrix(data = c(1.0, 0.0, 0.0, 0.0, 0.0,
                       0.0, 0.25, 0.25, 0.25, 0.25,
                       0.0, 0.0, 0.0, 0.0, 1.0), nrow = 5)
  answer_m <- matrix(data = c(1.0, 0.0, 0.0, 0.0, 0.0,
                              0.0, 0.3125, 0.3125, 0.3125, 0.0625,
                              0.0, 0.0, 0.0, 0.0, 1.0), nrow = 5)
  costs <- seq(1:ncol(m))
  priors <- seq(1:nrow(m)) / sum(seq(1:nrow(m)))

  expect_that(rsa.reason(m, depth = 2), is_equivalent_to(rsa.reason(rsa.reason(m)))) #! recursion is correct
  expect_that(rsa.reason(m, costs = costs, depth = 2),
              is_equivalent_to(rsa.reason(rsa.reason(m, costs = costs), costs = costs)))
  expect_that(rsa.reason(m, priors = priors, depth = 2),
              is_equivalent_to(rsa.reason(rsa.reason(m, priors = priors), priors = priors)))
})
test_that("rsa.reason passed valid values", {
  m <- matrix(data = c(1.0, 0.0, 0.0, 0.0, 0.0,
                       0.0, 0.25, 0.25, 0.25, 0.25,
                       0.0, 0.0, 0.0, 0.0, 1.0), nrow = 5)
  fakeM <- data.frame(m)
  badCosts1 <- c(1:(ncol(m)-1))
  badPriors1 <- c(1:(nrow(m)+1))
  expect_error(rsa.reason(fakeM), "'m' should be a matrix")
  expect_error(rsa.reason(m, costs = badCosts1), "Incorrect cost vector dimensions")
  expect_error(rsa.reason(m, priors = badPriors1), "Incorrect priors vector dimensions")
  expect_error(rsa.reason(m, depth = -1), "Invalid depth amount, must be an integer ge 0")
  expect_error(rsa.reason(m, depth = -2.5), "Invalid depth amount, must be an integer ge 0")
  expect_error(rsa.reason(m, alpha = -1), "Invalid alpha amount, must be numerical expression strictly greather than 0")
  expect_error(rsa.reason(m, alpha = c(1, 2)), "Invalid alpha amount, must be numerical expression strictly greather than 0")
  expect_error(rsa.reason(m, alpha = "apple"), "Invalid alpha amount, must be numerical expression strictly greather than 0")
})
