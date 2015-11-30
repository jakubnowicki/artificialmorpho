# tworzy sztuczną populację na odstwie modelu

sample.population <- function(model,population.size = 10,Csize = c(10,100),curves = NULL, symmetry = FALSE, mid = NULL, 
                              left = NULL, right = NULL, random.sd = 0.0001,elements = FALSE, elements.list = NULL) {
  sample.Csize <- runif(n = population.size, min = Csize[1], max = Csize[2])
  output <- NULL
  for (i in 1:population.size) {
    tmp <- generate.predicted(model = model,predictor = sample.Csize[i],random.sd = random.sd,elements = elements,elements.list = elements.list)
    tmp <- two.d.matrix(tmp)
    output <- abind::abind(output,tmp,along = 3)
  }
  if (symmetry == TRUE) {
    output <- symetria.2(dane = output,mid = mid,left = left,right = right)
  }
  output.gpg <- gpagen(output,ShowPlot = FALSE,curves = curves)
  output.gpg$Csize <- sample.Csize
  return(output.gpg)
}