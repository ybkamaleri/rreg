##' Histogram with comparison to national data
##'
##' Create a histogram for each health institutions or health regions
##' compared to the national data.
##'
##' @param data Data set
##' @param x x-axis
##' @param y y-axis
##' @param flip Flip plot horizontally
##' @param show The category that will have distinct colour f.eg. National mean
##' @param cut Where to split to show text inside or outside the bar eg. all under 10
##'   will show text outside bar while all above 10 will show text inside bar
##' @param sort Sort data ascending order
##' @param title Title for the plot
##' @param ylab Label for y-axis
##' @param xlab Label for x-axis
##'
##' @import ggplot2
##' @export

reghist <- function(data, x, y, flip = FALSE) {

  if (missing(data)) {
    stop("'data' must be provided",
         call. = FALSE)
  }

  data$xvar <- data[, deparse(substitute(x))]
  data$yvar <- data[, deparse(substitute(y))]


  ## Theme uten title axis-y
  theme2 <- theme_bw() +
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
  col2 <- c("#6baed6","#0845ff")

  p <- ggplot(data) +
    geom_bar(aes(x = reorder(xvar, yvar), yvar), stat = "identity") +
    theme2

  ## Flip plot
  if (flip) {
    p <- p + coord_flip()
  }

  return(p)
}
