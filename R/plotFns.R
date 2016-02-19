#' Plot literal listener semantics
#' Compare dimensions of two matrices or vectors
#' @param m1, matrix 1
#' @param processedData, data frame after proceessing (from calling processData())
#' @return, fill this out
#' @keywords plotting
#' @examples
#' d <- data(practiceData)
#' processedData <- processData(d)
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
    ggplot2::facet_wrap(as.formula(paste("~", labels[["group"]]))) +
    ggplot2::xlab(labels[["quantity"]]) +
    ggplot2::ylab(labels[["semantics"]]) +
    ggplot2::scale_colour_discrete(name = labels[["item"]])
}
