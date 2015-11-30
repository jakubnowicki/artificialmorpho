# tworzzy model na podstawie prwdziwych landmarków

generate.model <- function(data) {
  data.m <- two.d.array(data$coords)
  size <- data$Csize
  n <- ncol(data.m)
  model <- lm(as.matrix(data.m)[,1:n] ~ size)
  return(model)
}