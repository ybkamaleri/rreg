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

  ###################################################
  ## Prepare and restructure data set
  ###################################################

  ## data
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
  ## dummy ref row for text
  ref.row <- dfrow + 1
  xdf$ref <- ref.row

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
    ypos <- 0.15 * max(data$ylocal, na.rm = TRUE)
    ymax <- max(data$ylocal, na.rm = TRUE)
  } else {
    ypos <- 0.15 * max(data$ycomp, na.rm = TRUE)
    ymax <- max(data$ycomp, na.rm = TRUE)
  }

  ##################################
  ## other parameters for plotting
  ##################################

  ## positioning of text for table
  ytxt <- ypos + ymax

  ## conditions for y-axis break
  if (ymax < 11) {
    ybreak <- 2
    yline <- ymax
  } else if (ymax < 51) {
    ybreak <- 5
    yline <- ymax
  } else {
    ybreak <- round(0.2 * ymax, -1)
    yline_end <- 0.05 * ytxt
    yline <- round(ytxt - yline_end, -1) #extend y-axis and -1 to round to nearest 10
  }

  ##gap between n and N
  ygap <- 0.1 * ymax

  ##lenght of grid line
  ygrid <- ymax + (0.05 * ymax)

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
    geom_segment(aes(x = ref, xend = ref,
                     y = ygrid, yend = 0), #if yline used line can overlap when big numbers
                 size = 0.5, color = "grey70",
                 linetype = "dashed", lineend = "butt") +
    ## cover up the grid for dummy line
    geom_segment(data = data[data$ref == ref.row, ],
                 aes(x = ref, xend = ref, y = ygrid, yend = 0), #if yline used line can overlap when big numbers
                 size = 0.8, color = "white",
                 lineend = "butt") +
    geom_bar(aes(ref, ylocal), stat = "identity") +
    geom_point(aes(ref, ycomp), stat = "identity",
               shape = 18, size = 6, color = "blue") +
    coord_flip() +
    scale_x_discrete(breaks = factor(data$ref), labels = data$xvar)

  ## justification for table text
  tjust <- 1 #0 left, 1 right and 0.5 middle

  ## plot with theme and axis text
  p <- p + ptheme +
    theme(axis.line = element_blank()) +
    ## expand=c(0,0) used to place text close to axis
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, yline, ybreak)) +
    geom_segment(aes(y = 0, yend = yline, x = -Inf, xend = -Inf))

  ## Table
  if (tab){
    p <- p +
      geom_text(aes(ref, ytxt, label = ylocal), hjust = tjust) +
      geom_text(aes(ref, ytxt + ygap, label = ycomp), hjust = tjust) +
      annotate("text", x = ref.row, y = ytxt, label = "(n)", hjust = tjust) +
      annotate("text", x = ref.row, y = ytxt + ygap, label = "(N)", hjust = tjust)
  }

  return(p)

}
