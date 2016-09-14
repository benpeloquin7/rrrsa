#' Tune depth and alpha hyperparamters
#'
#' Return a list with number of alpha * depths elements
#' each element includes a tuple of (correlation, alpha, depth).
#' Same basic call functionality as \code{runDf()}
#' @param data, tidied data
#' @param quantityVarName, entity name we're quantifying over
#' @param semanticsVarName, semantic values for inference computation
#' @param itemVarName, unique items were comparing, probaby words
#' @param groupName, grouping variable if we have one
#' @param compareDataName, pragmatic judgments we're comparing to
#' @param costsVarName, costs variable name
#' @param priorsVarName, priors variable name
#' @param depths, vector of depths (in integers) for tuning
#' @param alphas, vector of alphas for tuning
#' @param compareIndices, specific indices in data frame
#' @param usePriorEveryRecurse, boolean incorporate priors during each full recursion
#' @importFrom stats "na.omit"
#' @importFrom stats "cor"
#' @return list of length(alphas) * length(depths) tuples with (correlation, depth, alpha)
#' @keywords data tuning
#' @export
#' @examples
#' d <- peloquinFrank_5Alts
#' alphas <- seq(1, 3, by = 0.1)
#' depths <- 1:3
#' checkWords <- c("some", "all", "good", "excellent", "liked", "loved", "memorable", "unforgettable",
#' "palatable", "delicious")
#' compareIndices <- which(peloquinFrank_5Alts$words %in% checkWords)
#' results <- rsa.tuneDepthAlpha(data = d, groupName = "scale",
#' quantityVarName = "stars", itemVarName = "words",
#' semanticsVarName = "speaker.p", compareDataName = "e11",
#' compareIndices = compareIndices, alphas = alphas, depths = depths)
#' head(results)
#' best <- which.max(unlist(lapply(results, function(i) i[[1]][1])))
#' results[[best]]
#'
rsa.tuneDepthAlpha <- function(data, quantityVarName, semanticsVarName, itemVarName, groupName = NA, compareDataName,
                               costsVarName = NA, priorsVarName = NA, depths = 1, alphas = 1, compareIndices = NA,
                               usePriorEveryRecurse = TRUE) {

  cors <- data.frame(cor = NA, depth = NA, alpha = NA)
  ## running multiple groups
  if (!is.na(groupName)) {
    for (a in alphas) {
      for (d in depths) {
        currRun <- plyr::ddply(data, .fun = rsa.runDf, .variables = c(groupName),
                               quantityVarName = quantityVarName,
                               semanticsVarName = semanticsVarName,
                               itemVarName = itemVarName,
                               costsVarName = costsVarName,
                               priorsVarName = priorsVarName,
                               depth = d, alpha = a, usePriorEveryRecurse = usePriorEveryRecurse)
        if (length(compareIndices) == 1 & is.na(compareIndices[1])) {
          res <- c(cor = cor(currRun[, compareDataName],
                             currRun[, "preds"], use = "pairwise.complete.obs"),
                   depth = d, alpha = a)
          cors <- rbind(cors, res)
          }
        else {
            compareData <- currRun[compareIndices, ]
            # compareData <- subset(currRun, words %in% compareItems)
            res <- c(cor = cor(compareData[, compareDataName],
                               compareData[, "preds"], use = "pairwise.complete.obs"),
                     depth = d, alpha = a)
            cors <- rbind(cors, res)
        }
      }
    }
  }
  cors <- na.omit(cors)
  cors[order(-cors$cor), ]
}

#' Run RSA on a tidied data frame subset (by group or on DF with one grouping var)
#'
#' Expects tidied data with three required (1-3) and two optional fields (4, 5)
#'
#' 1) quanityVarName         :: entity name we're quantifying over
#'
#' 2) itemVarName            :: unique items were considering,
#'
#' 3) semanticsVarName       :: literal listener semantics
#'
#' 4) optional costsVarName  :: costs
#'
#' 5) optional priorsVarName :: priors
#'
#' rsa-ready, tidied data should have a unique semantic value for each quantity * item combination.
#' (see formatting of peloquinFrank_2Alts data).
#' @param data, tidied data
#' @param quantityVarName, entity name we're quantifying over
#' (i.e. "stars" in Peloquin & Frank (2016))
#' @param semanticsVarName, literal listener semantic values for RSA computations
#' @param itemVarName, unique items were comparing, probaby words
#' (i.e. "degrees" in Peloquin & Frank (2016))
#' @param costsVarName, costs variable name
#' @param priorsVarName, priors variable name
#' @param alpha, decision noise parameter level
#' @param depth, recursive depth parameter
#' @param usePriorEveryRecurse, boolean incorporate priors during each full recursion
#' @return data frame with posterior predictions 'preds' column appended
#' @keywords primary run functionality
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' rsa.runDf(peloquinFrank_2Alts, "stars", "speaker.p", "words")
#'
rsa.runDf <- function(data,
                      quantityVarName,
                      semanticsVarName,
                      itemVarName,
                      costsVarName = NA,
                      priorsVarName = NA,
                      depth = 1,
                      alpha = 1,
                      usePriorEveryRecurse = FALSE) {

  ## `Not in` helper
  `%!in%` <- function(check, src) {
    !(check %in% src)
  }

  ## validation checks
  ## -----------------
  ## basic check for three neccessary specifications
  ##
  if (any(c(quantityVarName, semanticsVarName, itemVarName) %!in% names(data))) {
    stop("Cannot find column specification for quantity OR semantics OR items")
  }
  if (!is.numeric(unlist(dplyr::select_(data, semanticsVarName)))) stop("runDf expects semantics to be a numeric quantity")
  if (!is.na(costsVarName) & costsVarName %!in% names(data)) stop("Cannot find costs column")
  if (!is.na(priorsVarName) & priorsVarName %!in% names(data)) stop("Cannot find priors column")

  ## initial data processing
  ## -----------------------
  ##
  originalData <- data                #! save original data
  matrixLabels <- c(quantityVarName,  #! three required fields
                    semanticsVarName,
                    itemVarName)

  ## semantics data
  ## --------------
  ## Convert df to matrix
  ## Matrix info ->
  ##   cols = items (words),
  ##   rows = quantities (stars),
  ##   values = semantics (L0 probs)
  ##
  matrixData <- data %>%
    dplyr::select_(quantityVarName, semanticsVarName, itemVarName) %>%
    tidyr::spread_(itemVarName, semanticsVarName) %>%
    as.data.frame()
  rownames(matrixData) <- matrixData[[quantityVarName]]
  matrixData <- matrixData %>%
    dplyr::select(-1) %>%
    data.matrix()

  ## costs data
  ## ----------
  ##
  ## 1) assume uniform costs (0) if not present in data set
  if (is.na(costsVarName)) {
    costs <- rep(0, length(unique(data[[itemVarName]])))
    names(costs) <- unique(data[[itemVarName]])
  }
  ## 2) else create new (named) vector
  else {
    costsData <- data %>%
      dplyr::select_(itemVarName, costsVarName) %>%
      unique()
    costs <- costsData[[costsVarName]]
    names(costs) <- costsData[[itemVarName]]
  }
  #! costs validation check here

  ## priors data
  ## ------------
  ##
  ## 1) assume uniform (1) priors if not present in data set
  if (is.na(priorsVarName)) {
    priors <- rep(1, length(unique(data[[quantityVarName]])))
    names(priors) <- unique(data[[quantityVarName]])
  }
  ## 2) else create new (named) priors vector
  else {
    priorsData <- data %>%
      dplyr::select_(quantityVarName, priorsVarName) %>%
      unique()
    priors <- priorsData[[priorsVarName]]
    names(priors) <- priorsData[[quantityVarName]]
    quantityVec <- priorsData[[quantityVarName]] #! store this to repopulate during tyding
  }
  #! priors validation check here

  ## run rsa, compute posteriors
  posteriors <- rsa.reason(matrixData,
                           depth = depth,
                           alpha = alpha,
                           costs = costs,
                           priors = priors,
                           usePriorEveryRecurse = usePriorEveryRecurse)

  ## tidy data
  tidyPosterior <- as.data.frame(posteriors) %>%
    dplyr::mutate(quantityVarName = rownames(posteriors)) %>% #! add back quantity (stored in rownames)
    tidyr::gather(itemVarName, "preds", -quantityVarName)

  ## ensure col type matching before merge
  tidyPosterior[,"quantityVarName"] <- rsa.convertVecType(originalData[[quantityVarName]],
                                                          tidyPosterior[["quantityVarName"]])
  tidyPosterior[,"itemVarName"] <- rsa.convertVecType(originalData[[itemVarName]],
                                                      tidyPosterior[["itemVarName"]])
  # tidyPosterior[,"quantityVarName"] <- rsa.convertVecType(originalData[, quantityVarName],
  #                                                         tidyPosterior[, "quantityVarName"])
  # tidyPosterior[,"itemVarName"] <- rsa.convertVecType(originalData[, itemVarName],
  #                                                         tidyPosterior[, "itemVarName"])

  ## rename columns lost during dplyr
  renamedDf <- tidyPosterior %>%
    rsa.renameCol(c("quantityVarName", "itemVarName"),
                  c(quantityVarName, itemVarName))


  ## join with original data set
  mergedData <- suppressMessages(suppressWarnings(dplyr::left_join(originalData, renamedDf)))
  mergedData
}

#' Rename df columns
#'
#' return a data frame with columns renamed
#' (if they exist in the df passed in)
#' @param df, data frame to change col names
#' @param currNames, names we want to replace (passed as characters)
#' @param replacements, values used as replacements
#' @return data frame with changed col names
#' @export
#' @keywords data processing
#' @examples
#' df <- data.frame(LOWER_CASE = letters[1:3], upper_case = LETTERS[1:3])
#' rsa.renameCol(df, c("LOWER_CASE", "upper_case"), c("lower_case", "UPPER_CASE"))
#'
rsa.renameCol <- function(df, currNames, replacements) {
  if (!any(names(df) %in% currNames)) warning("Please review colnames passsed, no matches found.")
  indices <- which(names(df) %in% currNames) #! get df indices for cols
  names(currNames) <- replacements           #! store 'replacements' in attr
  for (i in indices) {
    names(df)[i] <-  #! for each name to replace, get name attr
      names(currNames[which(currNames == names(df)[i])])
  }
  df
}

#' Normalize vectors
#'
#' Return a normalized vector
#' @param v, vector to be normalized
#' @return a normalized vector
#' @keywords data processing
#' @export
#' @examples
#' v <- rep(0.8, 5)
#' rsa.normVec(v)
#'
rsa.normVec <- function(v) {
  if (!is.numeric(v) | any(sapply(v, function(i) i < 0))) stop("rsa.normVec expects positive numerical vector")
  normalizer = sum(v)
  if (normalizer == 0) rep(0, length(v))
  else (v / normalizer)
}

#' Convert vector data type
#'
#' @param vec1, first vector with type we want to match
#' @param vec2, second vector with type we'd like to transform
#' @export
#' @return vec2 with type matched to vec1
#' @examples
#' v1 <- as.factor(c("one", "two", "three"))
#' v2 <- c("one", "two", "three")
#' v3 <- c("1", "2", "3")
#' rsa.convertVecType(v1, v2)
#' rsa.convertVecType(v1, v3)
#'
rsa.convertVecType <- function(vec1, vec2) {
  if (typeof(vec1) == typeof(vec2)) vec2
  else if (is.factor(vec1)) as.factor(vec2)
  else if (is.integer(vec1)) as.integer(vec2)
  else if (is.character(vec1)) as.character(vec2)
  else vec2
}
