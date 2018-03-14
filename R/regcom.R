##' Table for plot
##'
##' Create table with column for dummy reference for plot with table
##'
##' @param data Data frame
##' @param x x-axis
##' @param ... Additional arguments
##'
##' @import ggplot2
##'
##' @export

regtab <- function(data, x, ...) {

  ## data frame
  if(missing(data)) {stop("Data must be provided", call. = FALSE)}

  ## x-axis
  if(missing(x)) {stop("'x' must be provided", call. = FALSE)}
  data$xvar <- data[, as.character(substitute(x))]

  ## New col for reference
  dfrow <- nrow(data)
  data$ref <- seq.int(dfrow)

  ## New DF for extra row to include text eg. N or Total
  dfcol <- names(data)
  xdf <- stats::setNames(data.frame(matrix(ncol = length(dfcol), nrow = 1)),
                         dfcol)
  refdf <- dfrow + 1
  xdf$ref <- refdf

  ## replace NA to "" to avoid NA is printed in the x-axis
  xdf$xvar <- ""

  ## Combine data and new DF
  data <- base::rbind(data, xdf)

  return(data)
}
