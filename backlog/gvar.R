
# Efficient calculation groupwise variances
gvar <- function(x, group, reorder = TRUE, na.rm = FALSE, ...){
  stopifnot(nrow(x) == length(group))
  nc <- ncol(x)
  rs <- rowsum(cbind(1, x, x^2), group, reorder = reorder, na.rm = na.rm, ...)
  nms <- rownames(rs)
  what <- rep(1:3, c(1, nc, nc))
  rs <- list(n = rs[, 1, drop = TRUE],
             x = as.matrix(rs[, what == 2L, drop = FALSE]),
             xsq = as.matrix(rs[, what == 3L, drop = FALSE]))
  with(rs, (xsq - x^2/n) / (n-1))
}

if (FALSE){             
  x <- iris[1:4]
  group <- iris$Species
  
  gvar(x, group)
  aggregate(x, list(group), var)
  
  xm <- as.matrix(x)
  gvar(xm, group)
  
  library(microbenchmark)
  microbenchmark(gvar(x, group), gvar(xm, group), aggregate(x, list(group), var))
  
  
  n <- 1e6
  x <- matrix(rnorm(2*n), n)
  xd <- as.data.frame(x)
  g <- as.integer(sample(round(n/2), n, replace = TRUE))
  
  system.time(res1 <- gvar(x, g))
  system.time(res2 <- gvar(xd, g))
  system.time(res3 <- aggregate(x, list(g), var))
  
  all.equal(res1, res2, check.attributes = FALSE)
  all.equal(as.vector(res1), as.vector(unlist(res3[-1], use.names = FALSE)))
  
}