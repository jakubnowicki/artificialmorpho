#' Search for elements
#'
#' Search for elements in semilandmark matrix.
#'
#' @param curves curves matrix
#' @export

# szuka serii landmarków lezących na współnych krzywych - łączy krzywe łączce się wspólnymi landmarkami

find.elements <- function(curves) {
  curves.list <- matrix.to.list(curves,break.points = find.curves(curves))
  i <- 1
  while (i < length(curves.list)+1) {
    com <- rep(FALSE,length(curves.list))
    for (j in (i+1):length(curves.list)) {
      com[j] <- common.elements(curves.list[[i]],curves.list[[j]])
    }
    if (any(com==TRUE)) {
      com.num <- which(com==TRUE)
      curves.list[[i]] <- c(unlist(curves.list[i]),unlist(curves.list[com.num]))
      curves.list <- curves.list[-com.num]
    }
    i <- i+1
  }
  for (i in 1:length(curves.list)) {
    curves.list[[i]] <- unique(curves.list[[i]])
  }
  return(curves.list)
}
