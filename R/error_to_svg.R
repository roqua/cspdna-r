#' Create a minimal svg on the basis of an error message (string)
#'
#' @param input A string of an error message.
#' @return A string of input potentially with linebreaks
#' @import stringr
#' @export
error_to_svg <- function(input) {
  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 480"><g>',
    '<rect fill="#ffffff" stroke="red" stroke-width="2" x="5" y="5" width="780" height="80" />',
    '<text x="10" y="50" font-size="24" fill="red">', 
    'GEEN GRAFIEKEN GEGENEREERD [',
    input, 
    ']',
    '</text>',
    '</g></svg>',
    collapse = ""
  )
}