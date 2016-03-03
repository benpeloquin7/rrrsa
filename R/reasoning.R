#' Run RSA model on processed data
#'
#' Only accepts processed data as from processData()
#' @param data, data passed after call to \code{processData()}
#' @param alpha, alpha parameter
#' @param depth, depth of recursion
#' @return, fill this out
#' @keywords main
#' @export
#' @examples
#' print("not yet implemented")
#'
rsa.batchRun <- function(data, alpha = 1, depth = 1) {
  # ------- Data verification checks here ------- #
  # ------- Data verification checks here ------- #

  # Prep data subgroups
  runData <- data$runData
  originalData <- data$orginalData
  groups <- unique(runData$group)
  originalLabels <- data$labels

  # predictions DF for rsa
  preds <- data.frame()
  for (g in groups) {
    # convert to matrix
    mData <- runData %>%
      dplyr::filter(group == g) %>%
      convertDf2Matrix()

    # Run rsa (currently not handling costs)
    modelPreds <- rsa.reason(m = mData, depth = depth, alpha = alpha) %>%
      convertMatrix2Df(., group = g)

    preds <- rbind(preds, modelPreds)
  }
  predDf <- preds %>%
    tidyr::gather(key = item, value = modelPreds, -c(group, quantity)) %>%
    unnameRSACols(originalLabels = originalLabels)

  # Add predictions column and return original data
  merge(data$originalData, removeNACols(predDf))
}


#' Run (multiple) iterations RSA \code{rsa.fullRecursion()}
#'
#' Return matrix after undergoing 'depth' recursions
#' @param m, matrix of semantics
#' @param costs, length m vector of costs (default is 0 valued vector)
#' @param priors, default uniform, vector of length nrow() [semantic quantity]
#' @param depth, number of recursions
#' @param alpha, decision hyper-parameter
#' @return, return a matrix of posteriors
#' @keywords recursion
#' @export
#' @examples
#' m <- matrix(data = c(0, 0.2, 0.25, 0.25, 0.3, 0, 0, 0, 0.3, 0.7), nrow = 5)
#' rownames(m) <- 1:5
#' colnames(m) <- c("item1", "item2")
#' rsa.reason(m, 0)
#' rsa.reason(m, 2)
#' priors <- c(0.1, 0.1, 0.1, 0.1, 0.6)
rsa.reason <- function(m, costs = rep(0, ncol(m)), priors = rep(1, nrow(m)), depth = 1, alpha = 1) {
  ## Validation checks
  ## Costs correspond to items (cols)
  ## Priors correspond to semantics (rows)
  if (length(costs) != ncol(m)) stop("Incorrect cost vector dimensions")
  if (length(priors) != nrow(m)) stop("Incorrect priors vector dimensions")

  while(depth > 0) {
    m <- rsa.fullRecursion(m, costs, priors, alpha)
    depth <- depth - 1
  }
  m
}

#' Run one full recursion between listener -> speaker -> listener
#'
#' @param m, matrix of semantics (rows = meaning (m rows), cols = words (n cols))
#' @param costs, length m vector of costs (default is 0 valued vector)
#' @param priors, default uniform, vector of length nrow() [semantic quantity]
#' @param alpha, decision hyper-param
#' @return, fill this out
#' @keywords recursion
#' @export
#' @examples
#' m <- matrix(data = c(0, 0.2, 0.25, 0.25, 0.3, 0, 0, 0, 0.3, 0.7), nrow = 5)
#' rownames(m) <- 1:5
#' colnames(m) <- c("item1", "item2")
#' rsa.fullRecursion(m)
#' rsa.fullRecursion(rsa.fullRecursion(m))
#' costs <- matrix(data = c(rep(-0.2, 5), rep(-0.8, 5)), nrow = 5)
#' rsa.fullRecursion(m, costs)
#' rsa.fullRecursion(rsa.fullRecursion(m, costs), costs)
#'
rsa.fullRecursion <- function(m, costs = rep(0, ncol(m)), priors = rep(1, nrow(m)), alpha = 1) {
  ## Validation checks
  ## Costs correspond to items (cols)
  ## Priors correspond to semantics (rows)
  if (length(costs) != ncol(m)) stop("Incorrect cost vector dimensions")
  if (length(priors) != nrow(m)) stop("Incorrect priors vector dimensions")

  ## Store matrix naming labels
  rNames <- rownames(m)
  cNames <- colnames(m)

  ## Costs as matrix for easy use of mapply
  costsAsMatrix <- matrix(rep(costs, times = 1, each = nrow(m)), nrow = nrow(m))
  rownames(costsAsMatrix) <- rNames
  colnames(costsAsMatrix) <- cNames

  ## Likelihood (compute over rows) ------ :: p(u | m)
  likelihood <- t(mapply(rsa.utility, split(m, row(m)),
                                      split(costsAsMatrix, row(costsAsMatrix)), alpha = alpha))

  ## Priors ------------------------------ :: p(u | m) * p(m)
  likelihood <- apply(likelihood, 2, function(i) priors * i)

  # Normalization (compute over cols) ---- :: [p(u | m) * p(m)] / (\sum_m p(m | u) * p(m))
  posterior <- mapply(rsa.utility, split(likelihood, col(likelihood)),
                                   split(costsAsMatrix, col(costsAsMatrix)), alpha = alpha)

  # Re-label matrix
  rownames(posterior) <- rNames
  colnames(posterior) <- cNames

  posterior
}

#' RSA utility
#'
#' Return normalized utility for a vector
#' @param items, literal semantic input vector <m_u1, m_u2, ...>
#' or <m1_u, m2_u,...>
#' @param alpha, decision nose parameter (see 'informativity()')
#' @param cost, cost vector <u1, u2, ...>
#' @return, fill this out
#' @export
#'
rsa.utility <- function(items, costs = rep(0, length(items)), alpha = 1) {
  ## Validation checks
  if (length(items) != length(costs)) stop("Item and cost dimensions do not match")

  normVec(mapply(rsa.informativity, items, costs, alpha = alpha))
}

#' RSA Informativity
#'
#' e^(-alpha * (-log(p(m|u)) - cost))
#' @param m_u, literal semantics of meaning given utterance
#' @param alpha, decision noise parameter (speaker's deviation
#' from optimal action selection)
#' @param cost, cost of utterance u
#' @return, fill this out
#' @keywords surprisal
#' @export
#' @examples
#' rsa.informativity(0.5, 1, 0) == 0.5
#' rsa.informatitivy(0, 0, 0) == 0.0
#'
rsa.informativity <- function(m_u, alpha = 1, cost = 0) {
  ## Validation checks
  if (m_u < 0 | m_u > 1) stop("Invalid semantic `m_u` value, must be between [0, 1]")
  if (alpha < 0) stop("Invalid alpha, must be a positive numerical expression")

  ## Watch for m_u == 0 in which case return 0
  ifelse(m_u == 0, 0, exp(-alpha * (-log(m_u) - cost)))
}
