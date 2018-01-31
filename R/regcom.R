##' Bar and point plot for local vs. national
##'
##' Create a barplot for local vs. national data on a specific measurement. This is to
##' differentiate local data compared to national data.
##'
##' @param data Data set
##' @param x x-axis
##' @param y y-axis
##' @param aim A line on y-axis indicating aim
##' @param ascending Sort data ascending order
##' @param title Title for the plot
##' @param ylab Label for y-axis
##' @param col1 Color for bars
##' @param col2 Color for the point
##' @param col3 Color for aim line
##' @param flip Flip plot horizontally
##' @param ... Additional arguments
##'
##' @import ggplot2
##'
##' @examples
##' # basic usage
##' library("rreg")
##'
##' @export

regcom <- function(data, x, y,
                   aim = NULL,
                   split = NULL,
                   ascending = TRUE,
                   title, ylab,
                   col1, col2, col3,
                   flip = TRUE,
                   ...) {

  ## missing data
  if (missing(data)) {
    stop("'data' must be provided",
         call. = FALSE)
  }

  ## missing x or y
  if (missing(x) | missing(y)) {
    stop("Both 'x' and 'y' should be specified",
         call. = FALSE)
  }

  ## x-axis
  data$xvar <- data[, as.character(substitute(x))]
  ## yvar
  data$yvar <- data[, as.character(substitute(y))]

  ## Title
  if (missing(title)){
    title <- ""
  } else {
    title = title
  }

  ## Label y-axis
  if (missing(ylab)){
    ylab <- substitute(y)
    ## ylab <- paste0("Pls specify eg. ylab = ", "\"", "Percentage", "\"")
  } else {
    ylab = ylab
  }

  ## Theme
  ptheme <- theme_bw() +
    theme(
      axis.text = element_text(size = 10), #text for y and x axis
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(size = 0.5),
      axis.title.y = element_blank(), #no title in y axis
      axis.title.x = element_text(size = 12),
      plot.margin = unit(c(0, 2, 1,1), 'cm'),
      plot.title = element_text(size = 14),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank())

  ## Colour
  if (missing(col1)) {
    col1 <- "lightblue"
  } else {
    col1 = col1
  }

  if(missing(col2)){
    col2 <- "#6baed6"
    colmix <- c(col1, col2)
  } else {
    colmix <- c(col1, col2)
  }



}
