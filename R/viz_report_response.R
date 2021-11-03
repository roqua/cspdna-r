#' Creates first visualisation for feedback-report (response-rate)
#'
#' @param data A dataframe with raw data
#' @return An svg of visualisation
#' @export
viz_report_response <- function(variable) {

  responded <- sum(is.na(variable))
  total <- length(variable)
  resp <- 100 * responded / total
  
  title <- paste0(responded, " uit ", total, " metingen ingevuld")
  
  ggplot(data = data.frame(x = 0, y = resp), aes(x = x, y = y)) +
    geom_bar(data = data.frame(x = 0, y = 100), 
             stat = "identity", fill = "#4F71BE", colour = "darkgrey") +
    geom_bar(stat = "identity", fill = "#DF8344") +
    geom_text(aes(label = paste0(round(resp), "%")), 
                  hjust = 1, colour = "white", size = 8) +
    theme_void() +
    coord_flip() +
    labs(title = title) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18)
    )

}
