##' Barplot with point and table for comparison
##'
##' Create a barplot with point to visualise comparison. It is also possible to
##' include table to show the value of the plot.
##'
##' @inheritParams regbar
##' @param yl Variable or column for local values
##' @param yc Variable or column for national values
##' @param tab Include table
##'
##' @import ggplot2
##'
##' @export

regcom <- function(data, x, yl, yc, tab = TRUE, ...) {

  ## Prepare and restructure data set
  ## data set
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

  ## y-axis for local
  if(missing(yl)) {stop("'yl' for local value must be provided", call. = FALSE)}
  data$ylocal <- data[, as.character(substitute(yl))]

  ## y-axis for national
  if(missing(yc)) {stop("'yc' for comparison value must be provided", call. = FALSE)}
  data$ycomp <- data[, as.character(substitute(yc))]


  ## table location
  if(max(data$ylocal, na.rm = TRUE) > max(data$ycomp, na.rm = TRUE)){
    ypos <- 0.1 * max(data$ylocal, na.rm = TRUE)
    ymax <- max(data$ylocal, na.rm = TRUE)
  } else {
    ypos <- 0.1 * max(data$ycomp, na.rm = TRUE)
    ymax <- max(data$ycomp, na.rm = TRUE)
  }

  ## other parameters for plotting
  ymax <- round(ymax, -1) #round ymax to nearest 10
  ytxt <- ypos + ymax
  yline <- ymax + (0.07 * ymax) #extend line 7% of ymax
  ybreak <- round(0.2 * ymax, -1)

  ## plot theme
  ptheme <- theme_classic() +
    theme(
      axis.text = element_text(size = 10), #text for y and x axis
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(size = 0.5),
      axis.title.y = element_blank(), #no title in y axis
      axis.title.x = element_text(size = 12),
      panel.grid.minor.x = element_blank())


  ## plot
  p <- ggplot(data) +
    geom_bar(aes(ref, ylocal), stat = "identity") +
    geom_point(aes(ref, ycomp), stat = "identity",
               shape = 18, size = 6, color = "blue") +
    coord_flip() +
    scale_x_discrete(breaks = factor(data$ref), labels = data$xvar)

  ## table
  p <- p + ptheme +
    geom_text(aes(ref, ytxt, label = ylocal), hjust = 1) +
    geom_text(aes(ref, ytxt + ypos, label = ycomp), hjust = 1) +
    annotate("text", x = refdf, y = ytxt, label = "(n)", hjust = 1) +
    annotate("text", x = refdf, y = ytxt + ypos, label = "(N)", hjust = 1) +
    ## expand=c(0,0) used to place text close to axis
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, ymax, ybreak))+
    geom_segment(aes(y = 0, yend = ymax, x = -Inf, xend = -Inf))+
    theme(axis.line = element_blank())

  return(p)

}
