#' normVec
#' Return a normalized vector
#' @param v, vector to be normalized
#' @keywords normalization
#' @examples
#' vec1 <- c(1, 1, 1)
#' norm(vec1)
#' vec2 <- c(0, 0, 0, 0)
#' norm(vec2)
#'
normVec <- function(v) {
  normalizer = sum(v)
  if (normalizer == 0) rep(0, length(v))
  else (v / normalizer)
}

#' validateDims
#' Compare dimensions of two matrices or vectors
#' @param m1, matrix 1
#' @param m2, matrix 2
#' @keywords safety_checks
#' @examples
#' m1 <- matrix(data = 1:4, nrow = 2)
#' m2 <- matrix(data = 5:8, nrow = 2)
#' m3 <- matrix(data = 5:10, nrow = 2)
#' validateDims(m1, m2)
#' validateDims(m1, m3)
#'
validateDims <- function(m1, m2) {
  type1 <- typeof(m1)
  type2 <- typeof(m2)
  if (type1 != type2) stop("Object types do not match")
  if (is.matrix(m1) & !all(dim(m1) == dim(m2))) stop("Matrix dimensions do not match")
  if (is.vector(m1) & (length(m1) != length(m2))) stop("Vector lengths do not match")
}
