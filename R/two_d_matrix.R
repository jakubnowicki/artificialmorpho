#' Matrix to array element
#'
#' Convert matrix into array element
#'
#' @param data matrix
#' @export

two.d.matrix <- function(data) {
  n.landmarks <- ncol(data)/2
  output <- matrix(0,ncol=2, nrow=n.landmarks)
  for (i in 1:n.landmarks) {
    output[i,1] <- data[1,(i*2)-1]
    output[i,2] <- data[1,(i*2)]
  }
  return(output)
}
