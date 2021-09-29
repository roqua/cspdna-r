#' Create label with linebreaks from input-string for graphical purposes
#'
#' @param input A string.
#' @return A string of input potentially with linebreaks
#' @export
comment_graph <- function(input) {
  if(is.null(input) | is.na(input)) {
    return(NA)
  } else {
    comm_len <- str_length(input)
    max_len <- 40
    if(comm_len <= max_len) {
      return(input)
    } else {
      output <- input
      no_lines <- ceiling(comm_len / max_len)
      no_chars_line <- ceiling(comm_len / no_lines)
      for(i in 1:(no_lines-1)) {
        extra_chars <- (i-1) * 2 # Compensate for adding "\n"
        index_char <- (i * no_chars_line) + extra_chars
        str_sub(output, index_char, index_char - 1) <- "\n"
      }
      return(output)
    }
  }
}
