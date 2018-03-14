##' Table for plot
##'
##' Create table with column for dummy reference for plot with table
##'
##' @inheritParams regbar
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
  data$ref <- as.factor(data$ref)

  return(data)
}


##' Barplot with point for comparison
##'
##' Create a barplot with point to visualise comparison. It is also possible to
##' include table to show the value of the plot.
##'
##' @inheritParams regbar
##' @param tab Include table
##'
##' @import ggplot2
##'
##' @export


regcom <- function(data, x, yl, yc, tab = TRUE, ...) {

  ## data frame
  if(missing(data)) {stop("Data must be provided", call. = FALSE)}
  ##:ess-bp-start::browser@nil:##

  ## x-axis
  if(missing(x)) {stop("'x' must be provided", call. = FALSE)}

  ## Prepare table with col reference
  data <- regtab(data, x)

  ## y-axis for local
  if(missing(yl)) {stop("'yl' for local value must be provided", call. = FALSE)}
  data$ylocal <- data[, as.character(substitute(yl))]

  ## y-axis for national
  if(missing(yc)) {stop("'yc' for comparison value must be provided", call. = FALSE)}
  data$ycomp <- data[, as.character(substitute(yc))]


  p <- ggplot(data, aes(ref, ylocal)) +
    geom_bar(stat = "identity") +
    geom_point(aes(ref, ycomp), stat = "identity",
               shape = 18, size = 8, color = "blue") +
    coord_flip() +
    scale_x_discrete(breaks = factor(data$ref), labels = data$xvar)

  return(p)


}
