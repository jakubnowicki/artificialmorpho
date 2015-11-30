# rozwiązuje równanie z 1 niewieadomoą w postaci y = ax + b

find.model.response.from.coeff <- function(interception, slope,x=NA,y=NA) {
  if (is.na(x) & is.na(y)) {
    stop("x or y value needed")
  }
  if (!is.na(x) & !is.na(y)) {
    stop("x, y - only one value needed")
  }
  if (!is.na(x)) {
    y <- slope * x + interception
    return(y)
  }
  else {
    x <- (y-interception)/slope
    return(x)
  }
}