#' normalize vectors
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

#' validate data entries
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

#' convert data to correct structure for conversion to matrix for reasoning()
#' data should be passed in 'long' format with 4 cols ->
#' 1) Group ::
#' 2) Quantity ::
#' 3) Item ::
#' 4) Semantics ::
#' @param data, data for running rsa
#' @param group, grouping variable (e.g. scales <some_all>)
#' @param quantity, quantity for assessing semantic compatibility (e.g. stars <1...5>)
#' @param item, invidiual word or degree level items (e.g. <strong, weak> or <hi, low> or <some, all>)
#' @param semantics, normalized distribution over quantity (e.g. compatibility DV)
#' @importFrom magrittr "%>%"
#' @importFrom tidyr gather
#' @export
#' @examples
#' d <- data.frame(groups = rep("some_all", 10),
#' quantity = rep(1:5, 2),
#' items = c(rep("strong", 5), rep("weak", 5)),
#' semantics = c(0, 0, 0, 0.3, 0.7,
#' 0, 0.1, 0.15, 0.35, 0.40),
#' pragmatics = c(0, 0, 0, 0.15, 0.85,
#' 0, 0.1, 0.25, 0.5, 0.15))
#' convertData(d, group = "groups", quantity = "quantity", item = "items", semantics = "semantics")
#'
convertData <- function(data, group = "scale", quantity = "stars", item = "degrees", semantics = "speaker.p") {

  # Verify valid data fields
  # ------------------------
  cols <- names(data)
  if (!all(group %in% cols &&
             quantity %in% cols &&
             item %in% cols &&
             semantics %in% cols)) {
    stop("Data set is missing fields")
  }

  # Verify valid dimensions
  # -----------------------
  # currently assuming we have groupings
  groupings <- unique(data$group)
  quantities <- unique(data$quantity)
  items <- unique(data$item)
  if (length(groupings) * length(quantities) * length(items) != length(semantics)) {
    stop("Invalid data dimensions")
  }

  # Convert to semantics for (near) matrix representation
  out <- data %>%
    select(groups, quantity, items, semantics) %>%
    spread(items, semantics)

  out
}
