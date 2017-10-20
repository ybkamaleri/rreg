##' Plot for variability
##'
##' Create a plot to show uncertainty either by showing the Standard Error of the
##' Mean (SEM) or Confidence Interval (CI). Lower and upper limit should be
##' specified. Figure should also be commented if the variability is a SEM or CI.
##'
##' @inheritParams regbar
##' @param ll Lower limit
##' @param ul Upper limit
##'
##' @export

regerr <- function(data, x, y,
                   ll, ul,
                   title, ylab,
                   col1, col2,
                   flip = TRUE,
                   ...){
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
  data$xvar <- data[, deparse(substitute(x))]
  ## yvar
  data$yvar <- data[, deparse(substitute(y))]

  ## Title
  if (missing(title)){
    title <- ""
  } else {
    title = title
  }

  ## Label y-axis
  if (missing(ylab)){
    ylab <- paste0("Pls specify eg. ylab = ", "\"", "Percentage", "\"")
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
    col3 <- c(col1, col2)
  } else {
    col3 <- c(col1, col2)
  }

}
