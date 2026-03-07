#' Regular Bar Chart with Automatic Label Placement
#'
#' Creates a horizontal (or vertical) bar chart using \code{ggplot2} with
#' intelligent label placement: labels are shown \strong{inside} the bar when
#' there is sufficient space, and automatically moved \strong{outside} when the
#' bar is too small to accommodate the text. No manual threshold tuning is
#' required — the function infers the correct placement from the rendered label
#' width relative to the bar length.
#'
#' @param data A \code{data.frame} containing the variables to plot.
#' @param x    Unquoted column name for the categorical axis (bar labels).
#' @param y    Unquoted column name for the numeric axis (bar lengths).
#' @param comp Character string (partial match) identifying one category to
#'   highlight with a contrasting fill colour (\code{col2}). If omitted all
#'   bars use \code{col1}.
#' @param num  Unquoted column name whose values are appended to the x-axis
#'   labels as \code{"(N = <value>)"}. Optional.
#' @param aim  Numeric scalar. When supplied, a dashed reference line is drawn
#'   at this y-value. Default \code{NULL} (no line).
#' @param ascending Logical. If \code{TRUE} (default) bars are sorted in
#'   ascending order of \code{y}.
#' @param title Character string for the plot title. Defaults to \code{""}.
#' @param ylab  Character string for the numeric-axis label. Defaults to the
#'   name of the \code{y} variable.
#' @param col1  Fill colour for ordinary bars. Defaults to
#'   \code{"lightblue"}.
#' @param col2  Fill colour for the highlighted bar selected by \code{comp}.
#'   Defaults to \code{"#6baed6"}.
#' @param col3  Colour for the \code{aim} reference line. Defaults to
#'   \code{"blue"}.
#' @param flip  Logical. If \code{TRUE} (default) the plot is flipped so bars
#'   run horizontally.
#' @param char_scale Numeric scaling factor that converts label character-count
#'   into axis-range units. Controls how generously space is estimated for each
#'   character. Defaults to \code{0.045}; increase (e.g. \code{0.06}) for
#'   larger text sizes, decrease (e.g. \code{0.03}) for smaller ones.
#' @param min_frac  Numeric. Minimum fraction of the axis range that a bar must
#'   span before its label is considered to fit inside. Acts as a safety floor
#'   for very short labels. Defaults to \code{0.08} (8 \%).
#' @param ... Currently unused; reserved for future arguments.
#'
#' @details
#' \strong{Universal label-placement rule}
#'
#' Rather than requiring the user to specify a fixed percentage threshold, the
#' function estimates the amount of axis space each label needs and compares
#' that estimate to the actual bar length:
#'
#' \enumerate{
#'   \item \code{y_range} is computed as
#'         \code{max(y) - min(0, min(y))}, capturing the full visible span of
#'         the axis including any negative region.
#'   \item For every observation the estimated label width (in axis units) is
#'         \code{nchar(label) * char_scale * y_range}.
#'   \item A per-bar threshold is \code{pmax(estimated_width, min_frac *
#'         y_range)}, ensuring even single-character labels have a sensible
#'         minimum floor.
#'   \item If \code{bar_value > threshold} the label is placed \emph{inside}
#'         (\code{hjust = 1.5}); otherwise it is placed \emph{outside}
#'         (\code{hjust = -0.5}).
#' }
#'
#' This logic works correctly across very different value ranges (e.g. raw
#' counts 0–50 000, proportions 0–1, percentages 0–100, or mixed positive /
#' negative values) without any manual \code{split} argument.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' df <- data.frame(
#'   region = c("North", "South", "East", "West", "Central"),
#'   pct    = c(42, 7, 88, 23, 3),
#'   n      = c(200, 30, 410, 115, 12)
#' )
#'
#' ## Basic usage — labels placed automatically
#' regbar(df, x = region, y = pct, title = "Coverage by Region",
#'        ylab = "Percentage (%)")
#'
#' ## With sample-size annotation and a highlighted bar
#' regbar(df, x = region, y = pct, num = n,
#'        comp = "North", aim = 50,
#'        title = "Coverage by Region", ylab = "Percentage (%)")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_text geom_hline labs
#'   scale_fill_manual scale_y_continuous coord_flip theme_bw theme
#'   element_text element_line element_blank unit
#' @export
regbar <- function(data, x, y,
                   comp, num, aim = NULL,
                   ascending  = TRUE,
                   title, ylab,
                   col1, col2, col3,
                   flip       = TRUE,
                   char_scale = 0.045, #Increase to 0.06 if using larger size=5 text
                   min_frac   = 0.08, #Increase if very short bars still feel cluttered
                   ...) {

  # ── Input validation ────────────────────────────────────────────────────────
  if (missing(data)) {
    stop("'data' must be provided", call. = FALSE)
  }
  if (missing(x) | missing(y)) {
    stop("Both 'x' and 'y' must be specified", call. = FALSE)
  }

  # ── Resolve column names ────────────────────────────────────────────────────
  data$xvar <- data[, as.character(substitute(x))]
  data$yvar <- data[, as.character(substitute(y))]

  # ── Optional title / axis labels ────────────────────────────────────────────
  if (missing(title)) title <- ""
  if (missing(ylab))  ylab  <- as.character(substitute(y))

  # ── x-axis label: optionally append N ──────────────────────────────────────
  if (missing(num)) {
    data$.xname <- data$xvar
  } else {
    num_col     <- as.character(substitute(num))
    data$.xname <- sprintf("%s (N=%s)", data$xvar, data[, num_col])
  }

  # ── Colours ─────────────────────────────────────────────────────────────────
  if (missing(col1)) col1 <- "lightblue"
  if (missing(col2)) col2 <- "#6baed6"
  if (missing(col3)) col3 <- "blue"
  colmix <- c(col1, col2)

  # ── Universal label-placement rule ──────────────────────────────────────────
  #
  #  The threshold for each bar is derived entirely from the data:
  #
  #  1.  y_range  = visible axis span (includes negative region if present)
  #  2.  For each observation, estimate the label's width in axis units as
  #          nchar(label) * char_scale * y_range
  #      where char_scale ≈ 0.045 is calibrated for geom_text size = 3.5.
  #  3.  Apply a minimum floor of min_frac * y_range so that even a 1-char
  #      label does not end up inside an extremely short bar.
  #  4.  ypos == 1  →  bar is long enough  →  label goes INSIDE
  #      ypos == 0  →  bar is too short    →  label goes OUTSIDE
  #
  y_range      <- max(data$yvar) - min(0, min(data$yvar))
  label_chars  <- nchar(as.character(data$yvar))
  label_width  <- label_chars * char_scale * y_range        # per-bar estimate
  threshold    <- pmax(label_width, min_frac * y_range)     # per-bar threshold
  data$ypos    <- ifelse(data$yvar > threshold, 1L, 0L)

  # ── Sort bars ────────────────────────────────────────────────────────────────
  if (ascending) {
    data$.xname <- factor(data$.xname,
                          levels = data$.xname[order(data$yvar)])
  }

  # ── Shared position / width ──────────────────────────────────────────────────
  pos   <- position_dodge(width = 0.80)
  wdth  <- 0.80

  # ── Theme ────────────────────────────────────────────────────────────────────
  ptheme <- theme_bw() +
    theme(
      axis.text        = element_text(size = 10),
      axis.ticks.y     = element_blank(),
      axis.line.x      = element_line(linewidth = 0.5),
      axis.title.y     = element_blank(),
      axis.title.x     = element_text(size = 12),
      plot.margin      = unit(c(0, 2, 1, 1), "cm"),
      plot.title       = element_text(size = 14),
      panel.background = element_blank(),
      panel.border     = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )

  # ── Base plot ────────────────────────────────────────────────────────────────
  p <- ggplot(data, aes(.xname, yvar))

  # ── Optional aim line ────────────────────────────────────────────────────────
  if (!is.null(aim)) {
    p <- p +
      geom_hline(yintercept = aim, color = col3,
                 linewidth = 1, linetype = "dashed")
  }

  # ── Bars ─────────────────────────────────────────────────────────────────────
  if (missing(comp)) {
    p <- p +
      geom_bar(width = wdth, stat = "identity",
               fill = col1, position = pos)
  } else {
    comp_match <- grep(comp, data$.xname, value = TRUE)
    p <- p +
      geom_bar(width = wdth, stat = "identity",
               aes(fill = .xname == comp_match),
               position = pos)
  }

  # ── Labels: inside (ypos == 1) or outside (ypos == 0) ───────────────────────
  inside  <- data[data$ypos == 1L, ]
  outside <- data[data$ypos == 0L, ]

  if (nrow(inside) > 0) {
    p <- p +
      geom_text(data     = inside,
                aes(label = yvar),
                hjust    = 1.5,
                position = pos,
                size     = 3.5)
  }
  if (nrow(outside) > 0) {
    p <- p +
      geom_text(data     = outside,
                aes(label = yvar),
                hjust    = -0.5,
                position = pos,
                size     = 3.5)
  }

  # ── Scales, labels, flip ─────────────────────────────────────────────────────
  p <- p +
    labs(title = title, y = ylab, x = "") +
    scale_fill_manual(values = colmix, guide = "none") +
    scale_y_continuous(expand = c(0, 0)) +
    ptheme

  if (flip) p <- p + coord_flip()

  return(p)
}
