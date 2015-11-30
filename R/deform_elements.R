# deformuje obiekt podzielony na elementy i wporwadza element losowych różnic w deformacji

deform.elements <- function(data,elements.list,a,theta,random.sd) {
  splited <- split.matrix(data,elements.list)
  n.elements <- length(splited)
  tmp.list <- list()
  for (i in 1:n.elements) {
    strain <- strain.matrix(a = a+rnorm(1,0,random.sd),theta = theta + rnorm(1,0,random.sd))
    tmp <- deformacja(data = splited[[i]],strain.matrix = strain)
    tmp.list[[i]]<-tmp
  }
  output <- do.call(rbind,tmp.list)
  output <- output[order(as.numeric(rownames(output))),]
  return(output)
}