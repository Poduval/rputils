# function for winsorizing data
# (the most extreme p/2 values on both sides are set equal the corresponding quantile)
winsorize <-
function(x, p = 0.1, na.rm = FALSE){
  up <- quantile(x, 1-p/2, na.rm = na.rm)
  lo <- quantile(x, p/2, na.rm = na.rm)
  if (is.finite(up)) x <- pmin(x, up)
  if (is.finite(lo)) x <- pmax(x, lo)
  x
  }
