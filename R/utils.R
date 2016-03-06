#' run RSA on a tidied data frame (assumes one group)
#'
#' Data should be passed in 'long' format with 4 required fields
#' 1) quanityVarName :: entity name we're quantifying over
#' (i.e. "stars" in Peloquin & Frank (2016))
#' 2) item :: unique items were compring, probaby words
#' 3) semantics ::
#' @param data, data for running rsa
#' @param quanityVarName, entity name we're quantifying over
#' (i.e. "stars" in Peloquin & Frank (2016))
#' @param itemVarName, unique items were compring, probaby words
#' (i.e. "degrees" in Peloquin & Frank (2016))
#' @param costsVarName, costs
#' @param priorsVarName, priors
#' @return, return data frame with 'pred' appended
#' @keywords run function
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#'
rsa.runDf <- function(data,
                      quantityVarName, semanticsVarName, itemVarName,
                      costsVarName = NA, priorsVarName = NA) {
  #! validation checks here

  ## initial data processing
  ## -----------------------
  originalData <- data
  originalColNames <- names(data)
  matrixLabels <- c(quantityVarName, semanticsVarName, itemVarName) #! these must be present
  matrixIndices <- match(matrixLabels, names(data))

  ## semantics data
  ## --------------
  ## cols = items (words),
  ## rows = quantities (stars),
  ## values = semantics (L0 probs)
  matrixData <- data %>%
    dplyr::select_(quantityVarName, semanticsVarName,
                   itemVarName) %>%
    tidyr::spread_(itemVarName, semanticsVarName)
  rownames(matrixData) <- matrixData[, quantityVarName] #! save names to rows
  matrixData <- matrixData %>%
    dplyr::select(-1) %>%
    data.matrix()

  ## costs data
  ## ----------
  ## 1) assume uniform costs (0) if not present in data set
  if (is.na(costsVarName)) {
    costs <- rep(0, length(unique(data[, itemVarName])))
    names(costs) <- unique(data[, itemVarName])
  }
  ## 2) else create new (named) vector
  else {
    costsData <- data %>%
      select_(itemVarName, costsVarName) %>%
      unique()
    costs <- costsData[, costsVarName]
    names(costs) <- costsData[, itemVarName]
  }
  #! costs validation check here

  ## priors data
  ## ------------
  ## 1) assume uniform (0) priors if not present in data set
  if (is.na(priorsVarName)) {
    priors <- rep(1, length(unique(data[, quantityVarName])))
    names(priors) <- unique(data[, quantityVarName])
  }
  # 2) else create new (named) priors vector
  else {
    priorsData <- data %>%
      select_(quantityVarName, priorsVarName) %>%
      unique()
    priors <- priorsData[, priorsVarName]
    names(priors) <- priorsData[, quantityVarName]
    quantityVec <- priorsData[, quantityVarName] #! store this to repopulate during tyding
  }
  #! priors validation check here

  ## run rsa to compuate posteriors
  posteriors <- rsa.reason(matrixData, costs = costs, priors = priors)

  ## tidy data
  tidyPosterior<- data.frame(posteriors) %>%
    dplyr::mutate(quantityVarName = rownames(.)) %>% #! add back quantity (stored in rows)
    tidyr::gather(itemVarName, "preds", -quantityVarName)

  ## rename columns lost during dplyr
  renamedDf <- tidyPosterior %>%
    rsa.renameCol(c("quantityVarName", "itemVarName"),
                  c(quantityVarName, itemVarName))

  ## join with original data set
  mergedData <- left_join(originalData, renamedDf)
  mergedData
}

#' Process data to correct structure for conversion to matrix for reasoning()
#'
#' Data should be passed in 'long' format with 4 required fields
#' 1) Group ::
#' 2) Quantity ::
#' 3) Item ::
#' 4) Semantics ::
#' @param data, data for running rsa
#' @param group, grouping variable (e.g. scales <some_all>)
#' @param quantity, quantity for assessing semantic compatibility (e.g. stars <1...5>)
#' @param item, invidiual word or degree level items (e.g. <strong, weak> or <hi, low> or <some, all>)
#' @param semantics, normalized distribution over quantity (e.g. compatibility DV)
#' @return, fill this out
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
#' processData(d, group = "scales", quantity = "stars", item = "degrees", semantics = "speaker.p")
#'
rsa.processData <- function(data, group = NA, quantity, item, semantics) {

  ## save original data
  originalData <- data
  ## get column names for data field verifications
  cols <- names(data)

  ## If group is NULL then proceed without grouping var
  ## i.e. this would be the same as testing on scalar family
  ## from Peloquin & Frank (2016)
  if (is.na(group)) {
#     # Verify valid data fields
#     if (!all(quantity %in% cols &&
#              item %in% cols &&
#              semantics %in% cols)) {
#       stop("Data set is missing fields")
#     }
#
#     # Rename for serialization and maintain old names
#     out <- renameRSACols(data, quantity, item, semantics)
#
  stop("group is NA")
  }
  else {
    # Verify valid data fields
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
  }
  # Important labels here, used to validate data passed to later fns()
  list(runData = runData, labels = out[[2]], originalData = originalData)
}
debug(rsa.processData)
rsa.processData(df, group = "scales", quantity = "stars", item = "degrees", semantics = "speaker.p")

#' Rename group, quantity, item and semantics columns in user df for use in RSA
#'
#' @param df, data frame of measurements
#' @param group, group name (e.g. "scales")
#' @param quantity, quantity we're quantifying over (e.g. "stars")
#' @param item, items we're examining (either individual words
#' ("some", "all", or degrees, "weak", "strong))
#' @param semantics, normalized compatibility measures
#' @return, fill this out
#' @seealso Called by \code{processData()}
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

#' Validate data entries
#'
#' Compare dimensions of two matrices or vectors
#' @param m1, matrix 1
#' @param m2, matrix 2
#' @return, fill this out
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


#' Convert an RSA data frame to matrix for running RSA
#'
#' @param df, data frame containing at least group, quantity and two more cols (for items)
#' @return, fill this out
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
#' @return, fill this out
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
  df <- as.data.frame(m) %>%
    dplyr::mutate(group = group, quantity = rownames(.))
  df
}

#' Return to original names (before renameRSACols()) for output
#'
#' @param df, data frame with renamed columns
#' @param oldNames, original names
#' @return, fill this out
#' @keywords data_org
#' @seealso Called by \code{run_rrrsa}
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
unnameRSACols <- function(df, originalLabels) {
  newDf <- df
  names(newDf)[names(newDf) == "group"] <-
    originalLabels[[which(names(originalLabels) == "group")]]
  names(newDf)[names(newDf) == "quantity"] <-
    originalLabels[[which(names(originalLabels) == "quantity")]]
  names(newDf)[names(newDf) == "item"] <-
    originalLabels[[which(names(originalLabels) == "item")]]
  names(newDf)[names(newDf) == "semantics"] <-
    originalLabels[[which(names(originalLabels) == "semantics")]]
  newDf
}


#' Remove columns named NA from data frame
#'
removeNACols <- function(df) {
  df[, -which(is.na(colnames(df)))]
}

#' Normalize vectors
#'
#' Return a normalized vector
#' @param v, vector to be normalized
#' @return, fill this out
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

#' Rename df columns avoiding NSE problems
#'
#' return a data frame with columns change
#' (if they exist in the df passed in)
#' @param df, data frame to change names
#' @param currNames, character names we want to replace
#' @param replacements, values used as replacements
#' @return, NA
#' @keywords data processing
#' @examples
#' print("make an example here")
#'
rsa.renameCol <- function(df, currNames, replacements) {
  indices <- which(names(df) %in% currNames)
  names(df)[indices] <- replacements[indices]
  df
}
