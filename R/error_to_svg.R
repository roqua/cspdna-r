#' Create a minimal svg on the basis of an error message (string)
#'
#' @param input A string of an error message.
#' @return A string of input potentially with linebreaks
#' @import stringr
#' @export
error_to_svg <- function(input) {
  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 480"><g>',
    '<rect fill="#ffffff" stroke="#000000" stroke-width="5" x="5" y="5" width="630" height="470" />',
    "<text x='80' y='100' font-size='24'>", 
    input,
    "</text>",
    '</g></svg>',
    collapse = ""
  )
}
