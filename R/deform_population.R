# tworzy zdeformowaną tektonicznie populację

deform.population <- function(data,min.a=0.8,max.a=1.2,min.theta=-0.5,max.theta=0.5,diff.elements = FALSE,curves = NULL,split.list=NULL,random.sd=NULL) {
  coords <- data$coords
  n.specimens <- dim(coords)[3]
  output.coords <- NULL
  for (i in 1:n.specimens) {
    a <- runif(1,min = min.a,max = max.a)
    theta <- runif(1, min = min.theta, max = max.theta)
    tmp.data <- coords[,,i]
    if (diff.elements==FALSE) {
      strain.m <- strain.matrix(a = a,theta = theta)
      tmp <- deformacja(data = tmp.data,strain.matrix = strain.m)
    }
    else {
      if (is.null(curves) & is.null(split.list)) {
        stop('Required curves matrix or split list.')
      }
      if (!is.null(curves)) {
        split <- split.list(data = tmp.data,curves = curves)
      } else {
        split <- split.list
      }
      tmp <- deform.elements(data = tmp.data,a = a,theta = theta,elements.list = split,random.sd = random.sd)
    }
    output.coords <- abind::abind(output.coords,tmp,along = 3)
  }
  output <- list(coords = output.coords, Csize = data$Csize)
  return(output)
}