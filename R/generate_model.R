#' Create linear model
#'
#' Creates linear of landmarks with size as predictor
#' @param data landmarks array
#' @export

generate.model <- function(data) {
  data.m <- two.d.array(data$coords)
  size <- data$Csize
  n <- ncol(data.m)
  model <- lm(as.matrix(data.m)[,1:n] ~ size)
  return(model)
}
