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

#' process data to correct structure for conversion to matrix for reasoning()
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
#' @keywords data_org
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' d <- data.frame(scales = rep("some_all", 10),
#' stars = rep(1:5, 2),
#' degrees = c(rep("strong", 5), rep("weak", 5)),
#' speaker.p = c(0, 0, 0, 0.3, 0.7,
#' 0, 0.1, 0.15, 0.35, 0.40),
#' listener.p = c(0, 0, 0, 0.15, 0.85,
#' 0, 0.1, 0.25, 0.5, 0.15))
#' convertData(d, group = "scales", quantity = "stars", item = "degrees", semantics = "speaker.p")
#'
processData <- function(data, group, quantity, item, semantics) {

  # save original data
  originalData <- data

  # Verify valid data fields
  cols <- names(data)
  if (!all(group %in% cols &&
             quantity %in% cols &&
             item %in% cols &&
             semantics %in% cols)) {
    stop("Data set is missing fields")
  }

  # Rename for serialization and maintain old names
  out <- renameRSACols(data, group, quantity, item, semantics)

  # Verify valid dimensions
  # (currently assuming we have groupings)
  groupings <- unique(data$group)
  quantities <- unique(data$quantity)
  items <- unique(data$item)
  if (length(groupings) * length(quantities) * length(items) != length(data$semantics)) {
    stop("Invalid data dimensions")
  }

  # Convert to semantics for (near) matrix representation
  runData <- out[[1]] %>%
    dplyr::select(group, quantity, item, semantics) %>%
    dplyr::mutate(semantics = as.numeric(semantics)) %>%
    tidyr::spread(item, semantics)

  # Important labels here, used to validate data passed to later fns()
  list(runData = runData, labels = out[[2]], originalData = originalData)
}

#' Convert an RSA data frame to matrix for running RSA
#' @param df, data frame containing at least group, quantity and two more cols (for items)
#' @keywords data_org
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#' @export
#' @examples
#' d <- data.frame(scales = rep("some_all", 10),
#' stars = as.factor(rep(1:5, 2)),
#' degrees = c(rep("strong", 5), rep("weak", 5)),
#' speaker.p = c(0, 0, 0, 0.3, 0.7,
#' 0, 0.1, 0.15, 0.35, 0.40),
#' pragmatics = c(0, 0, 0, 0.15, 0.85,
#' 0, 0.1, 0.25, 0.5, 0.15))
#' cData <- convertData(d)
#' convertDf2Matrix(cData)
#'
convertDf2Matrix <- function(df) {
  if (!("group" %in% names(df) && "quantity" %in% names(df))) stop("Invalid data passed")

  m <- df %>%
    dplyr::select(-c(group, quantity)) %>%
    as.matrix()
  m
}

#' Convert an RSA matrix to data frame for return to user
#' append group and quantity columns (for matching in return df)
#' @param m, matrix
#' @keywords data_org
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' d <- data.frame(scales = rep("some_all", 10),
#' stars = as.factor(rep(1:5, 2)),
#' degrees = c(rep("strong", 5), rep("weak", 5)),
#' speaker.p = c(0, 0, 0, 0.3, 0.7,
#' 0, 0.1, 0.15, 0.35, 0.40),
#' pragmatics = c(0, 0, 0, 0.15, 0.85,
#' 0, 0.1, 0.25, 0.5, 0.15))
#' cData <- convertData(d)
#' mData <- convertDf2Matrix(cData$runData)
#' convertMatrix2Df(mData)
#'
convertMatrix2Df <- function(m, group) {
  quantity <-
    df <- as.data.frame(m) %>%
    dplyr::mutate(group = group,
                  quantity = rownames(.))
  df
}

#' Rename group, quantity, item and semantics columns in user df for use in RSA
#' @param df, data frame of measurements
#' @param group, group name (e.g. "scales")
#' @param quantity, quantity we're quantifying over (e.g. "stars")
#' @param item, items we're examining (either individual words
#' ("some", "all", or degrees, "weak", "strong))
#' @param semantics, normalized compatibility measures
#' @keywords data_org
#' @export
#' @examples
#' d <- data.frame(scales = rep("some_all", 10),
#' stars = as.factor(rep(1:5, 2)),
#' degrees = c(rep("strong", 5), rep("weak", 5)),
#' speaker.p = c(0, 0, 0, 0.3, 0.7,
#' 0, 0.1, 0.15, 0.35, 0.40),
#' pragmatics = c(0, 0, 0, 0.15, 0.85,
#' 0, 0.1, 0.25, 0.5, 0.15))
#' newDf <- renameRSACols(d, group = "scales", quantity = "stars",
#' item = "degrees", semantics = "speaker.p")
#' newDf$data
#' newDf$labels
#'
renameRSACols <- function(df, group, quantity, item, semantics) {
  oldNames <- c(group = group, quantity = quantity, item = item, semantics = semantics)
  names(df)[names(df) == group] <- "group"
  names(df)[names(df) == quantity] <- "quantity"
  names(df)[names(df) == item] <- "item"
  names(df)[names(df) == semantics] <- "semantics"
  list(data = df, labels = oldNames)
}

#' Return to original names (before renameRSACols()) for output
#' @param df, data frame with renamed columns
#' @param oldNames, original names
#' @keywords data_org
#' @export
#' @examples
#' d <- data.frame(scales = rep("some_all", 10),
#' stars = as.factor(rep(1:5, 2)),
#' degrees = c(rep("strong", 5), rep("weak", 5)),
#' speaker.p = c(0, 0, 0, 0.3, 0.7,
#' 0, 0.1, 0.15, 0.35, 0.40),
#' pragmatics = c(0, 0, 0, 0.15, 0.85,
#' 0, 0.1, 0.25, 0.5, 0.15))
#' newDf <- renameRSACols(d, group = "scales", quantity = "stars",
#' item = "degrees", semantics = "speaker.p")
#' unnameRSACols(newDf$data, newDf$labels)
#'
unnameRSACols <- function(df, oldNames) {
  newDf <- df
  names(newDf)[names(newDf) == "group"] <- oldNames[[which(names(oldNames) == "group")]]
  names(newDf)[names(newDf) == "quantity"] <- oldNames[[which(names(oldNames) == "quantity")]]
  names(newDf)[names(newDf) == "item"] <- oldNames[[which(names(oldNames) == "item")]]
  names(newDf)[names(newDf) == "semantics"] <- oldNames[[which(names(oldNames) == "semantics")]]
  newDf
}
