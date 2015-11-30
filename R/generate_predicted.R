#' Create artificial landmark set
#'
#' Generates artificial landmark set based on model.
#'
#' @param model model
#' @param predictor predictor
#' @param random.sd standard deviation of coefficients randomness; default = 0.0001
#' @param elements randomization constant in elements; default = FALSE
#' @param elements.list optional list of elements; default = NULL
#' @export
#' @import RetroGeoMorph
#' @import morphoutils
#' @import geomorph
#' @import abind

generate.predicted <- function(model, predictor, random.sd = 0.0001,elements=FALSE,elements.list=NULL) {
  coefficients.matrix <- model$coefficients
  output.n <- ncol(coefficients.matrix)
  if (elements==FALSE) {
    coefficients.matrix[1,] <- coefficients.matrix[1,]+rnorm(output.n,0,random.sd)
    coefficients.matrix[2,] <- coefficients.matrix[2,]+rnorm(output.n,0,random.sd)
  } else {
    n.elements <- length(elements.list)
    for (i in 1:n.elements) {
      coeff.1 <- rnorm(1,0,random.sd)
      coeff.2 <- rnorm(1,0,random.sd)
      coefficients.matrix[1,elements.list[[i]]] <- coefficients.matrix[1,elements.list[[i]]] + coeff.1
      coefficients.matrix[2,elements.list[[i]]] <- coefficients.matrix[2,elements.list[[i]]] + coeff.2
    }
  }
  output <- matrix(0,nrow=1,ncol=output.n)
  for (i in 1:output.n) {
    coeff <- as.vector(coefficients.matrix[,i])
    output[1,i] <- find.model.response.from.coeff(interception = coeff[1],slope = coeff[2],x = predictor)
  }
  return(output)
}
