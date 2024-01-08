#' Create ggplot-visualisation of 'circle' with all symptoms
#'
#' @param data A dataframe in pre-specified format through "prepare_data"
#' @param output String ("poster" or "slider") indicating which labels are presented
#' @return A ggplot-object ('circle' visualisation)
#' @import dplyr ggplot2 tidyr
#' @export
viz_nw <- function(data, output = "poster") {
  # Vector with variable names of negative emotions
  negatief <- c("Bedroefd", "Boos", "Bang", "Energie", "Spanning", "Zelfbeeld",
                "Terugtrekken", "Destructief", "Suicidaliteit", "Onrustig", "Bijzondere_ervaringen",
                "Negatief_contact", "Lichamelijke_klachten", "Onplezierig")
  
  # Vector with variable names of positive emotions
  positief <- c("Blij", "Ondernemen", "Contact_behoefte", "Ervaren_controle",
                "Zorg_zelf", "Activiteiten", "Verplichtingen", "Slapen", "Plezierig")
  
  # Create long dataframe
  long <- data %>%
    # Select variables
    select(all_of(c("Datum","csp_dna_fase", "dayno", "pertwee",
                    positief, negatief))) %>%
    # From wide to long
    gather(all_of(c(positief, negatief)), key = "Var", value = "Score") %>%
    # Create new variable
    mutate(pos_neg = case_when(Var %in% positief ~ "positief",
                               Var %in% negatief ~ "negatief"))
  
  # Vector with variable names
  vars_meas <- c(negatief, positief)
  
  # Create dataframe with layout of nodes in a circle
  no_nodes <- length(vars_meas)
  if (no_nodes == 2) {
    node_df <- data.frame(Name = vars_meas,
                          x = c(0,0),
                          y = c(1, -1),
                          stringsAsFactors = FALSE)
  } else {
    node_df <- data.frame(Name = vars_meas,
                          x = sin(2 * pi * ((0:(no_nodes - 1))/no_nodes)),
                          y = cos(2 * pi * ((0:(no_nodes - 1))/no_nodes)),
                          stringsAsFactors = FALSE)
  }
  
  # Combine coordinates nodes with data
  long <- dplyr::left_join(long, node_df, by = c("Var" = "Name"))
  
  # Create abbreviations of node names
  long$abbr <- tolower(substr(long[["Var"]], 1, 4))
  
  # Base plot
  plot <- ggplot(long, aes(x = .data[["x"]], y = .data[["y"]])) +
    geom_point(size = 10, colour = "lightgrey", na.rm = TRUE) +
    scale_x_continuous(expand = c(0.20, 0)) +
    #scale_y_continuous(expand = c(0.20, 0)) +
    coord_fixed() +
    facet_wrap(~Datum, nrow = 1) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.key = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      strip.background = element_rect(fill = NA, colour = "grey50"),
      panel.border = element_blank(),
      panel.grid = element_blank()
    ) +
    # Add coloured circles depending on score
    geom_point(aes(size = .data[["Score"]], colour = .data[["pos_neg"]]), na.rm = TRUE) +
    scale_size_area(max_size = 10) +
    scale_colour_manual(values = c("#CC79A7", "#56B4E9")) +
    # Add variable label in circle
    geom_text(aes(label = abbr), colour = "white") +
    guides(colour = "none", size = "none") +
    # add bar for colour fase
    geom_rect(data = filter(long, !is.na(csp_dna_fase)),
              aes(xmin = -Inf, xmax = Inf,
                  ymin = -1.21, ymax = -1.26, fill = factor(csp_dna_fase)), inherit.aes = FALSE) +
    scale_fill_manual(values = c("1" = "green3", "2" = "yellow2",
                                 "3" = "darkorange", "4" = "firebrick2"), 
                      guide = "none") +
    ylim(c(-1.27, 1.27)) 
  
  
  if(output == "slider") {
    # Create labels for slider
    # Create label dataframe
    df_label <- data %>%
      # Select variables
      select(all_of(c("Datum", "csp_dna_55a", "csp_dna_56a", "csp_dna_57a"))) %>%
      # Create better labels
      mutate(
        csp_dna_55a_v = map_chr(csp_dna_55a, comment_graph),
        csp_dna_56a_v = map_chr(csp_dna_56a, comment_graph),
        csp_dna_57a_v = map_chr(csp_dna_57a, comment_graph)
      )
    
    # Add event labels for slider
    return(
      plot +
        # top label
        geom_label(data = filter(df_label, !is.na(csp_dna_55a_v)),
                   aes(x = 0, y = 0.6, label = csp_dna_55a_v),
                   lineheight = 0.72, size = 3) + 
        geom_label(data = filter(df_label, !is.na(csp_dna_56a_v)),
                   aes(x = 0, y = 0.1, label = csp_dna_56a_v),
                   lineheight = 0.72, size = 3) +
        geom_label(data = filter(df_label, !is.na(csp_dna_57a_v)),
                   aes(x = 0, y = -0.40, label = csp_dna_57a_v),
                   lineheight = 0.72, size = 3) 
    )
  } else {
    # Create labels for poster
    # Create label dataframe
    df_label <- data %>%
      # Select variables
      select(all_of(c("Datum", "csp_dna_55a", "csp_dna_77a", "csp_dna_78a"))) %>%
      # Create better labels
      mutate(
        csp_dna_55a_v = map_chr(csp_dna_55a, comment_graph),
        csp_dna_77a_v = map_chr(csp_dna_77a, comment_graph),
        csp_dna_78a_v = map_chr(csp_dna_78a, comment_graph)
      )
    
    # Add event labels for poster
    return(
      plot +
        # top label
        geom_label(data = filter(df_label, !is.na(csp_dna_55a_v)),
                   aes(x = 0, y = 0.6, label = csp_dna_55a_v),
                   lineheight = 0.72, size = 3) + 
        geom_label(data = filter(df_label, !is.na(csp_dna_77a_v)),
                   aes(x = 0, y = 0.1, label = csp_dna_77a_v),
                   lineheight = 0.72, size = 3) +
        geom_label(data = filter(df_label, !is.na(csp_dna_78a_v)),
                   aes(x = 0, y = -0.5, label = csp_dna_78a_v),
                   lineheight = 0.72, size = 3) 
    )
  }
}
