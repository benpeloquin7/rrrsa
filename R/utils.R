#' normVec
#' Return a normalized vector
#' @param v, vector to be normalized
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
