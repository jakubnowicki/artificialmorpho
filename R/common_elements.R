#' Common elements
#'
#' Finds if vectors have common elements.
#'
#' @param a first vector
#' @param b second vector
#' @export

common.elements <- function(a,b) {
  x <- a %in% b
  out <- any(x==TRUE)
  return(out)
}
