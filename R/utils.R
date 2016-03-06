#' run RSA on a tidied data frame (assumes one group)
#' --------------------------------------------------
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
#' @param costsVarName, costs variable name
#' @param priorsVarName, priors variable name
#' @return, return data frame with 'pred' appended
#' @keywords run function
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#'
rsa.runDf <- function(data, quantityVarName, semanticsVarName, itemVarName,
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
    dplyr::mutate(quantityVarName = rownames(.)) %>% #! add back quantity
                                                     #! (stored in rows from rsa.reason())
    tidyr::gather(itemVarName, "preds", -quantityVarName)

  ## rename columns lost during dplyr
  renamedDf <- tidyPosterior %>%
    rsa.renameCol(c("quantityVarName", "itemVarName"),
                  c(quantityVarName, itemVarName))

  ## join with original data set
  mergedData <- left_join(originalData, renamedDf)
  mergedData
}

#' Rename df columns avoiding NSE problems
#' ---------------------------------------
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

#' Remove columns named NA from data frame
#' --------------------------------------
#'
rsa.removeNACols <- function(df) {
  df[, -which(is.na(colnames(df)))]
}

#' Normalize vectors
#' -----------------
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
