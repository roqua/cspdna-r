#' Create poster of all measurements, from roqua inputs
#' @param answers {csp_dna: [{id: 1, .. }]}, see tests/testthat/jsons/answers.json
#' @export
poster <- function(answers) {
  data <- prepare_data(JSON_to_DF(answers))
  
  if( is.character(data) ) {
    return(list(errors = data))
  } 
  
  viz_poster(data)
};

#' Svg's for feedback report.
#' @param answers {csp_dna: [{id: 1, .. }]}, see tests/testthat/jsons/answers.json
#' @export
report <- function(answers) {
  data <- prepare_data(JSON_to_DF(answers))
  
  if( is.character(data) ) {
    return(list(errors = data))
  } 
  
  errors <- c()
  svgs <- list()
  for(graph in c("viz_report_alone", "viz_report_behaviour", "viz_report_grid", "viz_report_response",  "viz_carousel")) {
    res <- do.call(graph, list(data = data))
    errors = c(errors, res$errors)
    svgs = c(svgs, res$svgs)
  }

  list(
    errors = errors,
    svgs = svgs
  )
};
