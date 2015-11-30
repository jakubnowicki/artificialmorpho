common.elements <- function(a,b) {
  x <- a %in% b
  out <- any(x==TRUE)
  return(out)
}