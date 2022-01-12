#' opencpu thing to return value as a scalar instead of array.
#' NULL -> NA
#' not written by writer of package, but by someone at Roqua
#'
#' @param obj Any object
#' @return NA (if NULL) or obj as a scalar
#' @export
as.scalar2 <- function(obj){
  if (is.null(obj)) obj <- NA
  return(structure(obj, class=c("scalar",class(obj))))
}