#' informativity
#' e^(-alpha * (-log(p(m|u)) - cost))
#' @param m_u, literal semantics of meaning given utterance
#' @param alpha, decision noise parameter (speaker's deviation
#' from optimal action selection)
#' @param cost, cost of utterance u
#' @keywords surprisal
#' @export
#' @examples
#' informativity(0.5, 1, 0) == 0.5
#' informatitivy(0, 0, 0) == 0.0
#'
informativity <- function(m_u, alpha = 1, cost = 0) {
  ifelse(m_u == 0, 0, exp(-alpha * (-log(m_u) - cost)))
}

#' utility
#' Return normalized utility for a vector
#' @param items, literal semantic input vector <m_u1, m_u2, ...>
#' or <m1_u, m2_u,...>
#' @param alpha, decision nose parameter (see 'informativity()')
#' @param cost, cost vector <u1, u2, ...>
#' @export
#'
utility <- function(items, costs = rep(0, length(items)), alpha = 1) {
  validateDims(items, costs)
  normVec(mapply(informativity, items, costs, alpha = alpha))
}

#' recurse
#' one full recursion between listener -> speaker -> listener
#' @param m, matrix of semantics (rows = meaning, cols = words)
#' @param fn, action to perform during recurison either 'normVec'
#' for simple normalization or 'utility' to compute utility
#' @keywords recursion
#' @export
#' @examples
#' m <- matrix(data = c(0, 0.2, 0.25, 0.25, 0.3, 0, 0, 0, 0.3, 0.7), nrow = 5)
#' rownames(m) <- 1:5
#' colnames(m) <- c("item1", "item2")
#' recurse(m)
#' recurse(recurse(m))
#' costs <- matrix(data = c(rep(-0.2, 5), rep(-0.8, 5)), nrow = 5)
#' recurse(m, costs)
#' recurse(recurse(m, costs), costs)
#'
recurse <- function(m, costs = m - m, alpha = 1) {
  validateDims(m, costs)

  # Store before computation
  rNames <- rownames(m)
  cNames <- colnames(m)
  ######################################
  ## Not using utility over cols ->
  # newM <- apply(t(mapply(utility, split(m, row(m)), split(costs, row(costs)), alpha = alpha)), 2, normVec)
  ## Just normalizing ->
  # newM <- apply(t(apply(m, 1, fn, costs = costs)), 2, fn, costs = costs, alpha = alpha)
  ######################################
  # Compute over rows and cols
  overRows <- t(mapply(utility, split(m, row(m)), split(costs, row(costs)), alpha = alpha))
  overCols <- mapply(utility, split(overRows, col(overRows)), split(costs, col(costs)), alpha = alpha)
  rownames(overCols) <- rNames
  colnames(overCols) <- cNames
  overCols
}

#' reason
#' return matrix after undergoing 'depth' recursions
#' calling 'fn' for computation
#' @param m, matrix of semantics
#' @param depth, number of iterations
#' @param fn, computation during recursion
#' @keywords recursion
#' @export
#' @examples
#' m <- matrix(data = c(0, 0.2, 0.25, 0.25, 0.3, 0, 0, 0, 0.3, 0.7), nrow = 5)
#' rownames(m) <- 1:5
#' colnames(m) <- c("item1", "item2")
#' reason(m, 0)
#' reason(m, 2)
#'
reason <- function(m, depth, costs = m - m, alpha = 1) {
  validateDims(m, costs)

  while(depth > 0) {
    m <- recurse(m, costs, alpha)
    depth <- depth - 1
  }
  m
}

#' run_rsa
#' run rsa reasoning over data
#' @param data, data with n subsets of matrices
#' @param alpha, alpha parameter
#' @param depth, depth of recursion
#' @keywords main
#' @export
#' @examples
#' print("example needed")
#'
run_rrrsa <- function(data, alpha, depth) {
  pass
}
