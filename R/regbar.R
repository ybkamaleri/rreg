##' Barplot with comparison to national data
##'
##' Create a barplot with the posibility to differentiate a specific item compared to
##' the rest. This is useful in a situation when there is a need to show the total
##' value as compared to each items in the x-axis. A specific example related to the
##' Norwegian Health Registries is when the sum of each health institutions or health
##' regions compared to the national data.
##'
##' @param data Data set
##' @param x x-axis
##' @param y y-axis
##' @param diff Differentiate a specific bar from the rest for a clear comparison
##'   eg. National compared to the different districts
##' @param ascending Sort data ascending order
##' @param title Title for the plot
##' @param ylab Label for y-axis
##' @param col1 Color for bars
##' @param col2 Color for the 'diff' bar
##' @param flip Flip plot horizontally
##' @param ... Additional arguments
##'
##' @import ggplot2
##' @export

regbar <- function(data, x, y,
                   diff,
                   ascending = TRUE,
                   title, ylab,
                   col1, col2,
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

  ## Theme uten title axis-y
  ptheme <- theme_bw() +
    theme(
      axis.text = element_text(size = 10), #text for y and x axis
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(size = 0.5),
      axis.title.y = element_blank(),
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


  ## Value placement inside or outside bar
  ysplit <- with(data, 0.1 * max(yvar))
  data$ypos <- ifelse(data$yvar > ysplit, 1, 0)

  ## Show value
  ymax <- 0.03 * max(data$yvar)
  data$txtpos <- ifelse(data$ypos == 0, data$yvar + ymax, data$yvar - ymax)


  ## Ascending order of xvar according to yvar
  if (ascending) {
    data$xvar <- with(data, factor(xvar, levels = xvar[order(yvar)]))
  }

  ## Base plot
  p <- ggplot(data, aes(xvar, yvar))

  ## Diff bar
  if (missing(diff)) {
    p <- p + geom_bar(stat = 'identity', fill = col1)
  } else {
    p <- p + geom_bar(stat = 'identity', aes(fill = xvar == diff))
  }

  ## Plot
  p <- p +
    geom_text(aes(y = txtpos, label = yvar), size = 3.5) +
    labs(title = title, y = ylab, x = "") +
    scale_fill_manual(values = col3, guide = 'none') +
    scale_y_continuous(expand = c(0, 0)) +
    ptheme

  ## Flip plot
  if (flip) {
    p <- p + coord_flip()
  }

  return(p)
}
