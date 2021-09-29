#' Create poster of all measurements
#'
#' @param data A dataframe in pre-specified format through "prepare_data"
#' @param height height (dimension) of poster in XX
#' @param width width (dimension) of poster in XX
#' @return An svg of poster
#' @import patchwork grid
#' @importFrom dplyr filter
#' @export
viz_poster <- function(data, height = 60, width = 35) {

  no_fig <- unique(data[["pertwee"]])
  no_fig <- no_fig[!is.na(no_fig)]
 # data$interval_esm <-  data[["pertwee"]]

  ts1 <- viz_ts(data, left_right = "left")
  ts2 <- viz_ts(data, left_right = "right")

  svg(file = paste0("poster_", height,"x", width,".svg"), height = height, width = width)

  pushViewport(viewport(layout = grid.layout(ceiling(length(no_fig)/2), 2)))
  vplayout <- function(x, y) viewport(layout.pos.row = x,
                                      layout.pos.col = y)

  j = 1
  row = 1
  col = 1

  for (i in no_fig) {

    data_zoom <- filter(data, pertwee ==  i)

    combined <- (ts1 + viz_zoom(data_zoom) | ts2 + viz_zoom(data_zoom) ) / viz_nw(data_zoom) / viz_grid(data_zoom) + plot_layout(ncol = 1, heights = c(1, 2, 2))

    #ggsave(paste0("slider_", i,".svg"), combined, width = 36.8, height = 18.3, unit = "cm")

    if((j %% 2) == 0) { # if number is even
      col = 2
      print(combined, vp = vplayout(row, col))
      row = row + 1
      col = 1
      j = j + 1
    } else if((j %% 2) != 0) { # if number is odd
      col = 1
      print(combined, vp = vplayout(row, col))
      col = 2
      j = j + 1
    }
  }
  dev.off()

}
