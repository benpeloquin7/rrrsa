#' Run RSA on a tidied data frame (assumes one group)
#' --------------------------------------------------
#'
#' Expects tidied data with three required and two optional fields
#' 1) quanityVarName         :: entity name we're quantifying over
#' 2) itemVarName            :: unique items were considering,
#' 3) semanticsVarName       :: literal listener semantics
#' 4) optional costsVarName  :: costs
#' 5) optional priorsVarName :: priors
#' @param data, tidied data
#' @param quanityVarName, entity name we're quantifying over
#' (i.e. "stars" in Peloquin & Frank (2016))
#' @param itemVarName, unique items were compring, probaby words
#' (i.e. "degrees" in Peloquin & Frank (2016))
#' @param costsVarName, costs variable name
#' @param priorsVarName, priors variable name
#' @return, return data frame with 'pred' col appended
#' @keywords primary run functionality
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#'
rsa.runDf <- function(data, quantityVarName, semanticsVarName, itemVarName,
                      costsVarName = NA, priorsVarName = NA) {
  #! validation checks here

  ## initial data processing
  ## -----------------------
  originalData <- data                #! save original data
  matrixLabels <- c(quantityVarName,  #! three required fields
                    semanticsVarName,
                    itemVarName)

  ## semantics data
  ## --------------
  ## Matrix info ->
  ##   cols = items (words),
  ##   rows = quantities (stars),
  ##   values = semantics (L0 probs)
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
      dplyr::select_(itemVarName, costsVarName) %>%
      unique()
    costs <- costsData[, costsVarName]
    names(costs) <- costsData[, itemVarName]
  }
  #! costs validation check here

  ## priors data
  ## ------------
  ## 1) assume uniform (1) priors if not present in data set
  if (is.na(priorsVarName)) {
    priors <- rep(1, length(unique(data[, quantityVarName])))
    names(priors) <- unique(data[, quantityVarName])
  }
  ## 2) else create new (named) priors vector
  else {
    priorsData <- data %>%
      dplyr::select_(quantityVarName, priorsVarName) %>%
      unique()
    priors <- priorsData[, priorsVarName]
    names(priors) <- priorsData[, quantityVarName]
    quantityVec <- priorsData[, quantityVarName] #! store this to repopulate during tyding
  }
  #! priors validation check here

  ## run rsa, compute posteriors
  posteriors <- rsa.reason(matrixData, costs = costs, priors = priors)

  ## tidy data
  tidyPosterior<- data.frame(posteriors) %>%
    dplyr::mutate(quantityVarName = rownames(.)) %>% #! add back quantity (stored in rownames)
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
#'
#' return a data frame with columns renamed
#' (if they exist in the df passed in)
#' @param df, data frame to change col names
#' @param currNames, names we want to replace (passed as characters)
#' @param replacements, values used as replacements
#' @return data frame with changed col names
#' @keywords data processing
#' @examples
#' NA
#'
rsa.renameCol <- function(df, currNames, replacements) {
  indices <- which(names(df) %in% currNames)
  names(df)[indices] <- replacements[indices]
  df
}

#' Normalize vectors
#' -----------------
#'
#' Return a normalized vector
#' @param v, vector to be normalized
#' @return, normalized vector
#' @keywords data processing
#' @examples
#' vec1 <- c(1, 1, 1)
#' normVec(vec1)
#' vec2 <- c(0, 0, 0, 0)
#' normVec(vec2)
#'
rsa.normVec <- function(v) {
  normalizer = sum(v)
  if (normalizer == 0) rep(0, length(v))
  else (v / normalizer)
}
