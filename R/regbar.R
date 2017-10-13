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
##' @param xlab Label for x-axis
##' @param cut Where to split i.e to show text inside or outside the bar eg. all
##'   under 10 will show text outside bar while all above 10 will show text inside
##'   bar
##' @param flip Flip plot horizontally
##'
##' @import ggplot2
##' @export

regbar <- function(data, x, y,
                   diff,
                   ascending = TRUE,
                   title, ylab,
                   flip = FALSE) {

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
  col2 <- c("#6baed6","#0845ff")

  ## Show value
  data$ypos <- with(data, yvar - (0.05 * max(yvar)))

  ## Ascending order of xvar according to yvar
  if (ascending) {
    data$xvar <- with(data, factor(xvar, levels = xvar[order(yvar)]))
  }

  ## Base plot
  p <- ggplot(data, aes(xvar, yvar))


  ## Diff bar
  if (missing(diff)) {
    p <- p + geom_bar(stat = 'identity')
  } else {
    p <- p + geom_bar(stat = 'identity', aes(fill = xvar == diff))
  }

  ## Plot
  p <- p +
    geom_text(aes(y = ypos, label = yvar), size = 3.5) +
    labs(title = title, x = xlab, y = ylab) +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0, 0)) +
    ptheme

  ## ## example geom_bar
  ## ggplot(whyfig, aes(x=reorder(fig, pros), y = pros)) +
  ##   geom_bar(stat = 'identity', aes(fill = ReshNavn == 'Norge')) +
  ##   geom_text(aes(y = ypos, label = pros), size = 3.5) +
  ##   geom_text(data = whyfig[whyfig$ReshNavn == "Norge"], aes(y = ypos, label = pros), size = 3.5, color = "white") +
  ##   coord_flip() +
  ##   ##guides(fill = FALSE) +
  ##   labs(title = figtitle, y = ylab) +
  ##   scale_fill_manual(values = col2, guide = 'none') +
  ##   scale_y_continuous(expand = c(0,0)) +
  ##   theme2

  ## Flip plot
  if (flip) {
    p <- p + coord_flip()
  }

  return(p)
}
