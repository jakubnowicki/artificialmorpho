#' Split landmarks matrix into elements
#'
#' Splits landmark matrix into list of elements
#'
#' @param data matrix of landmarks
#' @param curves curves matrix
#' @export

split.list <- function(data,curves) {
  elements <- find.elements(curves)
  n.elements <- length(elements)
  n.landmarks <- nrow(data)
  l.sequence <- 1:n.landmarks
  free.landmarks <- l.sequence[-unlist(elements)]
  n.free.landmarks <- length(free.landmarks)
  elements.and.free <- elements
  for (i in (n.elements+1):(n.elements+n.free.landmarks)) {
    elements.and.free[[i]] <- free.landmarks[i-n.elements]
  }
  return(elements.and.free)
}
