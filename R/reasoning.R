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
  if (m_u < 0 | m_u > 1) stop("Invalid semantic `m_u` value, must be betwween [0, 1]")
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
#' @param costs, matrix of costs (default is 0)
#' @param priors, default uniform, vector of length nrow() [semantic quantity]
#' @param alpha, decision hyper-param
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
recurse <- function(m, costs = m - m, priors = rep(1, nrow(m)), alpha = 1) {
  validateDims(m, costs)

  # Store naming labels
  rNames <- rownames(m)
  cNames <- colnames(m)

  # Likelihood (compute over rows) ------ :: p(u | m)
  overRows <- t(mapply(utility, split(m, row(m)), split(costs, row(costs)), alpha = alpha))
  # Priors ------------------------------ :: p(u | m) * p(m)
  withPriors <- apply(overRows, 2, function(i) priors * i)
  # Normalization (compute over cols) --- :: [p(u | m) * p(m)] / (\sum_m p(m | u) * p(m))
  overCols <- mapply(utility, split(withPriors, col(withPriors)), split(costs, col(costs)), alpha = alpha)

  # Labels
  rownames(overCols) <- rNames
  colnames(overCols) <- cNames
  overCols
}

#' reason
#' return matrix after undergoing 'depth' recursions
#' calling 'fn' for computation
#' @param m, matrix of semantics
#' @param costs, cost matrix dimensions identical to semantic matrix
#' @param priors, prior vector same length as nrow(m)
#' @param depth, number of recursions
#' @param alpha, decision hyper-param
#' @keywords recursion
#' @export
#' @examples
#' m <- matrix(data = c(0, 0.2, 0.25, 0.25, 0.3, 0, 0, 0, 0.3, 0.7), nrow = 5)
#' rownames(m) <- 1:5
#' colnames(m) <- c("item1", "item2")
#' reason(m, 0)
#' reason(m, 2)
#' priors <- c(0.1, 0.1, 0.1, 0.1, 0.6)
reason <- function(m, costs = m - m, priors = rep(1, nrow(m)), depth = 1, alpha = 1) {
  validateDims(m, costs)
  validateDims(m[,1], priors)

  while(depth > 0) {
    m <- recurse(m, costs, priors, alpha)
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
#' @examples
#' print("not yet implemented")
#'
run_rrrsa <- function(data, alpha, depth) {
  "not yet implemented"
}
