#' Creates see-through band for zooming on time-series graph of all measurements
#'
#' @param data A dataframe in pre-specified format through "prepare_data" only from period zoomed-in on
#' @return A list with ggplot-layer that annotates graph with see-through "band" indicating period zoomed-in on
#' @export
viz_zoom <- function(data_zoom) {

  list(annotate("rect", xmin = min(data_zoom$Datum),
                xmax = max(data_zoom$Datum),
                ymin = -Inf, ymax = Inf, alpha = .2, fill = "blue") )

  }
