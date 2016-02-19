#' Plot literal listener semantics
#' Compare dimensions of two matrices or vectors
#' @param m1, matrix 1
#' @param processedData, data frame after proceessing (from calling processData())
#' @keywords plotting
#' @examples
#'
#' processedData <- processData(practiceData)
#' plotSemantics(processedData)
#'
plotSemantics <- function(processedData) {
  plotData <- processedData$originalData
  labels <- processedData$labels

  ggplot2::ggplot(plotData,
                  ggplot2::aes(x = plotData[[labels[["quantity"]]]],
                               y = plotData[[labels[["semantics"]]]],
                               col = plotData[[labels[["item"]]]])) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(as.formula(paste("~", labels[["group"]])))
}
