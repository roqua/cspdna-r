#' Create images for slides
#'
#' @param data A dataframe in pre-specified format through "prepare_data"
#' @param height height (dimension) of image in cm
#' @param width width (dimension) of image in cm
#' @return Multiple SVGs in string-format in list to be used in slider
#' @import patchwork ggplot2 svglite
viz_carousel <- function(data, height = 18.3, width = 36.8) {

  if( !is.data.frame(data) ) return(list(error = "Input not a dataframe"))  
  
  no_fig <- unique(data[["pertwee"]])
  no_fig <- no_fig[!is.na(no_fig)]

  ts1 <- viz_ts(data, left_right = "left")
  ts2 <- viz_ts(data, left_right = "right")
  
  slider_list <- list()
  
  for (i in no_fig) {
    
    # Creates relevant zoomed in version of data (specific time frame)
    data_zoom <- filter(data, pertwee ==  i)

    # Combine graphs for slider
    
    viz_z <- viz_zoom(data_zoom)
    combined <- (ts1 + viz_z | ts2 + viz_z ) / viz_nw(data_zoom, output = "slider") / viz_grid(data_zoom) + plot_layout(ncol = 1, heights = c(1, 2, 2)) 
    
    # Create string of svg
    viz_string <- svglite::svgstring(height = 18.3/2.4, width = 36.8/2.4)
    print(combined) 
    invisible(dev.off())

    # Add svg string to list    
    slider_list[[paste0("carousel_", i)]] <- as.scalar2(as.character(viz_string()))
  }
  return(list(svgs = slider_list))
}
