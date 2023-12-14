#' Create ggplot-visualisation of lines over time (time-series)
#'
#' @param data A dataframe in pre-specified format through "prepare_data"
#' @param left_right String ("left" or "right") indicating what sets of variables to plot
#' @return A ggplot-object (time-series visualisation)
#' @import ggplot2 dplyr
#' @importFrom tidyr gather
#' @export
viz_ts <- function(data, left_right) {
  
  if(left_right == "left") {
    vars_neg <- c("Boos", "Lichamelijke_klachten", "Onrustig", "Spanning")
    
    # This creates a "long" rather than "wide" dataset, necessary for visualisation
    long <- data %>%
      # Selecting relevant variables
      select(all_of(c("Datum", "dayno", "pertwee", vars_neg))) %>%
      # From wide to long
      gather(all_of(vars_neg), key = "Var", value = "Score") %>% 
      # Omit missing data
      na.omit()
    
    plot <- ggplot(long, aes(x = Datum, y = Score, colour = Var)) +
      theme_minimal() +
      geom_smooth(se = FALSE, span = 0.2, method = "loess", formula = "y ~ x") +
      labs(x = "Time", colour = NULL) +
      scale_colour_brewer(palette = "Set2") +
      theme(legend.position = "top",
            legend.margin = margin(t = 0, b = 0, unit = 'cm'))
    
  } else if(left_right == "right") {
    
    vars_pos <- c("Activiteiten", "Blij", "Verplichtingen", "Zorg_zelf")
    
    # This creates a "long" rather than "wide" dataset, necessary for visualisation
    long <- data %>%
      # Selecting relevant variables
      select(all_of(c("Datum", "dayno", "pertwee", vars_pos))) %>%
      # From wide to long
      gather(all_of(vars_pos), key = "Var", value = "Score") %>% 
      # Omit missing data
      na.omit()
    
    plot <- ggplot(long, aes(x = Datum, y = Score, colour = Var)) +
      theme_minimal() +
      geom_smooth(se = FALSE, span = 0.2, method = "loess", formula = "y ~ x") +
      labs(x = "Time", colour = NULL) +
      scale_colour_brewer(palette = "Set1") +
      theme(legend.position = "top",
            legend.margin = margin(t = 0, b = 0, unit = 'cm'))
    
  }
  
  return(plot)
}
