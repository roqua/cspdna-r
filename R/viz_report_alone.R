#' Creates first visualisation for feedback-report (alone by phase)
#'
#' @param data A dataframe in pre-specified format through "prepare_data"
#' @param output_format String ("svg" or "ggplot") defining whether output should be ggplot or svg
#' @return An svg of visualisation
#' @export
viz_report_alone <- function(data, output_format = "svg") {

  g <- data %>% 
    mutate(fase = factor(csp_dna_fase, levels = c(4, 3, 2, 1))) %>% 
    group_by(csp_dna_55_a0, fase, .drop = FALSE) %>% 
    summarise(n = n()) %>% 
    filter( !is.na(csp_dna_55_a0) & !is.na(fase) ) %>% 
    group_by(csp_dna_55_a0) %>% 
    mutate(perc = 100 * n / sum(n)) %>% 
    ggplot(aes(x = csp_dna_55_a0, y = perc, fill = fase)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_x_continuous(breaks = c(0, 1), labels = c("In gezelschap", "Alleen")) +
    scale_fill_manual(breaks = factor(c(1, 2, 3, 4)),
                      values = c("1" = "green3", "2" = "yellow2",
                                 "3" = "darkorange", "4" = "firebrick2")) +
    theme_minimal() +
    labs(x = NULL, y = "%") +
    theme(
      legend.position = "top",
      panel.grid = element_blank(),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16)
    ) +
    coord_flip()
  
  
  if(output_format == "ggplot") {
    g
  } else {
    svg(file = "viz_report_alone.svg", height = 2.5, width = 5)
    print(g)
    dev.off() 
  }
  
  
}
