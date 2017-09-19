##' Histogram with comparison to national data
##'
##' Create a histogram for each health institutions or health regions
##' compared to the national data.
##'
##' @param data Data set
##' @param x x-axis
##' @param y y-axix
##' @param hf list of names or institutions
##' @param sort Sort data ascending order
##' @title Title for the plot
##'
##' @import ggplot2
##' @export

reghist <- function(data, x, y, hf, sort = TRUE, title) {

  data <- data
  x <- x
  y <- y
  hf <- hf

  ## sort hf ascending order according to y value
  list(
    geom_bar(aes(x, y)),
    if (sort)
      geom_bar(aes(x = reorder(hf, y), y))
  )


  reg <- ggplot(data) +
    geom_bar(stat = 'identity', aes(fill = ReshNavn == 'Norge')) +
    geom_text(data = kollperfig[N > 5], aes(y = ypos, label = pros), size = 3.5) +
    geom_text(data = kollperfig[N < 6], aes(y = 1.5, label = "n<6"), size = 3.5) +
    ## geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),  width = .3, color = "blue",
    ##               position = position_dodge(.9)) +
    coord_flip() +
    ##guides(fill = FALSE) +
    labs(title = "", y = ylab) +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2


}
