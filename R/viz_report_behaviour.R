#' Creates first visualisation for feedback-report (response-rate)
#'
#' @param data A dataframe with raw data
#' @return An svg of visualisation
#' @export
viz_report_behaviour <- function(variable) {

  # ### Figuur 1
  # Pp001 begeeft zich het meest in fase 2 en daarna fase 3. Slechts 1 keer begeeft pp001 zich in fase 1 en 4. Wat opvalt in het figuur hieronder is dat pp001 vrijwel even hoog scoort op roze (negatieve) en blauwe(positieve) items ongeacht of pp001 in fase 2 of 3 is. Het is dus niet zo dat pp001 in fase 3 veel meer lichamelijke klachten of spanning heeft of duidelijk minder activiteiten onderneemt.
  # 
  # 
  # 
  # ```{r, message=FALSE, fig.width = 8,  fig.height= 7}
  # data %>% select(c(Datum, positief, negatief, "csp_dna_fase")) %>%
  #   gather(c(positief, negatief), key = "Var", value = "Score") %>%
  #   mutate(pos_neg = case_when(Var %in% positief ~ "positief",
  #                              Var %in% negatief ~ "negatief")) -> long
  # #
  # # ggplot(long, aes(x = fct_reorder(Var, -Score, na.rm = T), y = Score)) +
  # #   geom_boxplot() +
  # #   coord_flip() +
  # #   theme_minimal() +
  # #   labs(x = NULL) +
  # #   theme(panel.grid.major.y = element_blank())
  # #
  # data %>% filter(!is.na(csp_dna_fase)) %>%
  #   group_by(csp_dna_fase) %>% summarise(n_fase = n()) -> n_fase
  # 
  # ggplot(filter(long, !is.na(csp_dna_fase)),
  #        aes(x = fct_reorder(Var, -Score, na.rm = T), y = Score,
  #            colour = pos_neg)) +
  #   geom_point(stat = "summary", fun.y = "mean", size = 4) +
  #   geom_label(data = n_fase, aes(x = -Inf, y = -Inf, label = paste("n =", n_fase)),
  #              inherit.aes = FALSE, hjust = -0.1, vjust = -0.1, size = 6) +
  #   scale_colour_manual(breaks = c("positief", "negatief"), values = c("#CC79A7", "#56B4E9")) +
  #   guides(colour=FALSE) +
  #   coord_flip() +
  #   theme_linedraw() +
  #   labs(x = NULL, colour = NULL) +
  #   theme(panel.grid.major.y = element_blank(),
  #         axis.text = element_text(size = 12)) +
  #   facet_grid(~ csp_dna_fase)
  # 
  # 
  # #ggplot(long, aes(x = Score)) +
  # #  geom_histogram(binwidth = 10) +
  # #  theme_minimal() +
  # #  facet_wrap(~ fct_reorder(Var, Score, na.rm = T)) +
  # #  theme(panel.grid.minor = element_blank(),
  # #        panel.grid.major.x = element_blank())
  # ```

}
