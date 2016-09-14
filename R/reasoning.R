#' Run multiple iterations (recursions) of RSA
#'
#' Return matrix after undergoing number 'depth' recursions
#' @param m, matrix of semantics with 'items' cols and 'quantity' rows
#' @param costs, ncol(m) vector of costs (default is 0 valued vector)
#' @param priors, nrow(m) vector of priors (default is uniform)
#' @param depth, number of recursions
#' @param alpha, decision hyper-parameter
#' @param usePriorEveryRecurse, boolean incorporate priors during each full recursion, default is FALSE
#' @return matrix of posterior values
#' @keywords recursion
#' @export
#' @examples
#' m <- matrix(data = c(0, 0.2, 0.25, 0.25, 0.3, 0, 0, 0, 0.3, 0.7), nrow = 5)
#' rownames(m) <- 1:5
#' colnames(m) <- c("item1", "item2")
#' rsa.reason(m, depth = 0)
#' rsa.reason(m, depth = 2)
#'
rsa.reason <- function(m,
                       costs = rep(0, ncol(m)),
                       priors = rep(1, nrow(m)),
                       depth = 1, alpha = 1,
                       usePriorEveryRecurse = FALSE) {
  ## Validation checks
  ## ------------------
  ## passed a matrix
  if (!is.matrix(m)) stop("'m' should be a matrix")
  ## costs correspond to items (cols)
  if (length(costs) != ncol(m)) stop("Incorrect cost vector dimensions")
  ## priors correspond to semantics (rows)
  if (length(priors) != nrow(m)) stop("Incorrect priors vector dimensions")
  ## depth >= 0
  if ((depth < 0) | (depth %% 1 != 0)) stop("Invalid depth amount, must be an integer ge 0")
  ## alpha >= 0
  if (length(alpha) > 1 | !is.numeric(alpha) | any(alpha < 0)) {
    stop("Invalid alpha amount, must be numerical expression strictly greather than 0")
  }

  ## run recursions for 'depth' iterations
  depth_itr <- depth
  while(depth_itr > 0) {
    ## use unif priors
    ## if we don't include during every recursion and we're not at depth 1
    if (!usePriorEveryRecurse & depth_itr != 1) {
      m <- rsa.fullRecursion(m, costs, rep(1, nrow(m)), alpha)
    }
    ## use argument priors
    else {
      m <- rsa.fullRecursion(m, costs, priors, alpha)
    }
    depth_itr <- depth_itr - 1
  }
  m
}

#' Run one full recursion between listener -> speaker -> listener
#'
#' @param m, matrix of semantics (rows = meaning (m rows), cols = words (n cols))
#' @param costs, ncol(m) vector of costs (default is 0 valued vector)
#' @param priors, nrow(m) vector of priors (default is uniform)
#' @param alpha, decision hyper-param
#' @return matrix of posterior values
#' @keywords recursion
#' @export
#' @examples
#' m <- matrix(data = c(0, 0.2, 0.25, 0.25, 0.3, 0, 0, 0, 0.3, 0.7), nrow = 5)
#' rownames(m) <- 1:5
#' colnames(m) <- c("item1", "item2")
#' rsa.fullRecursion(m)
#' rsa.fullRecursion(rsa.fullRecursion(m))
#'
rsa.fullRecursion <- function(m,
                              costs = rep(0, ncol(m)),
                              priors = rep(1, nrow(m)),
                              alpha = 1) {
  ## Validation checks
  ## -----------------
  ## passed a matrix
  if (!is.matrix(m)) stop("'m' should be a matrix")
  ## costs correspond to items (cols)
  if (length(costs) != ncol(m)) stop("Incorrect cost vector dimensions")
  ## priors correspond to semantics (rows)
  if (length(priors) != nrow(m)) stop("Incorrect priors vector dimensions")
  ## Populate some col names if none given
  if (is.null(colnames(m))) colnames(m) <- 1:ncol(m)
  ## alphas >= 0
  if (length(alpha) > 1 | !is.numeric(alpha) | any(alpha < 0)) {
    stop("Invalid alpha amount, must be numerical expression strictly greather than 0")
  }

  ## store matrix naming labels
  rNames <- rownames(m)
  cNames <- colnames(m)

  ## costs as matrix for easy use of mapply
  costsAsMatrix <- matrix(rep(costs, times = 1, each = nrow(m)), nrow = nrow(m))
  if (is.null(names(costs))) colnames(costsAsMatrix) <- cNames  #! name cols
  else colnames(costsAsMatrix) <- names(costs)
  costsAsMatrix <- costsAsMatrix[, cNames]                      #! IMPORTANT: maintain col ordering with 'm'
  rownames(costsAsMatrix) <- rNames                             #! assign rownames, don't lose this data

  ## likelihood (compute over rows) ------ :: p(u | m)
  likelihood <- t(mapply(rsa.utility, split(m, row(m)),
                                      split(costsAsMatrix, row(costsAsMatrix)), alpha = alpha))

  ## priors ------------------------------ :: p(u | m) * p(m)
  likelihood <- apply(likelihood, 2, function(i) priors * i)

  ## normalization (compute over cols) --- :: [p(u | m) * p(m)] / (\sum_m p(u | m) * p(m))
  posterior <- apply(likelihood, 2, rsa.normVec)

  ## re-label matrix
  rownames(posterior) <- rNames
  colnames(posterior) <- cNames

  posterior
}

#' RSA utility
#'
#' Return normalized utility for a vector
#' @param items, literal semantic input vector <m_u1, m_u2,..., m_uN>
#' @param alpha, decision noise parameter (see 'informativity()')
#' @param costs, cost vector <u1, u2,..., uN>
#' @return return a vector of utilities
#' @export
#' @examples
#' literalSemantics <- c(0.0, 0.0, 0.3, 0.3, 0.4)
#' rsa.utility(items = literalSemantics)
#' costs <- c(0.0, 0.0, 0.2, 0.3, 0.4)
#' rsa.utility(items = literalSemantics, costs = costs)
#'
rsa.utility <- function(items, costs = rep(0, length(items)), alpha = 1) {
  ## Validation checks
  ## -----------------
  ##
  if (length(items) != length(costs)) stop("Item and cost dimensions do not match")
  if (length(alpha) > 1 | !is.numeric(alpha) | any(alpha < 0)) stop("Invalid alpha amount, must be numerical expression strictly greather than 0")

  rsa.normVec(mapply(rsa.informativity, items, costs, alpha = alpha))
}

#' RSA informativity
#'
#' Calculate: e^(-alpha * (-log(p(m|u)) - cost))
#' @param m_u, literal semantics of meaning given utterance
#' @param alpha, decision noise parameter (speaker's deviation
#' from optimal action selection)
#' @param cost, cost of utterance u
#' @return Informativity of utterance u given meaning m and cost
#' @keywords surprisal
#' @export
#' @examples
#' rsa.informativity(m_u = 0.5, alpha = 1, cost = 0) == 0.5
#' rsa.informativity(m_u = 0, alpha = 1, cost = 0) == 0.0
#' rsa.informativity(m_u = 0.5, alpha = 1, cost = 2)
#'
rsa.informativity <- function(m_u, alpha = 1, cost = 0) {
  ## Validation checks
  ## -----------------
  ##
  if (m_u < 0 | m_u > 1) stop("Invalid semantic 'm_u' value, must be between [0, 1]")
  if (alpha < 0) stop("Invalid alpha, must be a positive, non-zero numerical expression")

  ## Watch for m_u == 0 in which case return 0
  ifelse(m_u == 0, 0, exp(-alpha * (-log(m_u) - cost)))
}
