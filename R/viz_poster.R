#' Create poster of all measurements
#'
#' @param data A dataframe in pre-specified format through "prepare_data"
#' @param height height (dimension) of poster in XX
#' @param width width (dimension) of poster in XX
#' @return An svg of poster
#' @import patchwork grid svglite
#' @importFrom dplyr filter
viz_poster <- function(data, height = 1.6 * (29.7 / 2.54) * 3, width = 2.12 * (42 / 2.54)) {

  if( !is.data.frame(data) ) return(list(error = "Input not a dataframe"))
  
  no_fig <- unique(data[["pertwee"]])
  no_fig <- no_fig[!is.na(no_fig)]
 
  ts1 <- viz_ts(data, left_right = "left")
  ts2 <- viz_ts(data, left_right = "right")

  viz_string <- svglite::svgstring(fix_text_size = FALSE, height = height, width = width)
  
  pushViewport(viewport(layout = grid.layout(ceiling(length(no_fig)/2), 2)))
  vplayout <- function(x, y) viewport(layout.pos.row = x,
                                      layout.pos.col = y)

  j = 1
  row = 1
  col = 1

  for (i in no_fig) {

    data_zoom <- filter(data, pertwee ==  i)

    vis_z <- viz_zoom(data_zoom)
    
    combined <- (ts1 + vis_z | ts2 + vis_z ) / viz_nw(data_zoom) / viz_grid(data_zoom) + plot_layout(ncol = 1, heights = c(1, 2, 2))
    
    #ggsave(paste0("slider_", i,".svg"), combined, width = 36.8, height = 18.3, unit = "cm")

    if((j %% 2) == 0) { # if number is even
      col = 2
      plot(combined, vp = vplayout(row, col))
      row = row + 1
      col = 1
      j = j + 1
    } else if((j %% 2) != 0) { # if number is odd
      col = 1
      plot(combined, vp = vplayout(row, col))
      col = 2
      j = j + 1
    }
  }
  invisible(dev.off())
  #write(as.character(viz_string()), file = "poster_example_new.svg", append = FALSE, ncolumns = 1)
  list(svgs = list(poster = as.scalar2(as.character(viz_string()))))
}
