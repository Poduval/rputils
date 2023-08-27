
# ==== Auxiliary functions, used in other functions, not exported ====

# return length 2 combination number i out of the choose(n, 2) possibile combination of 
# two elements from 1:n
findPair <- function(i, n){
  .i <- 1
  .k <- n
  repeat {
    if (i<.k) break 
    i <- i-.k+1
    .i <- .i+1
    .k <- .k-1
    if (.k<=1) stop("Invalid input")
  }
  c(.i, .i+i)  
}
    
if (FALSE){ # Tests / examples
  findPair(1, 10)
  findPair(2, 10)
  findPair(23, 10)
  findPair(45, 10)
  findPair(46, 10)  # error

  t(sapply(1:45, findPair, n = 10))
}

# One draw from a multivariate hypergeometric distribution
# 
# urns  number of balls in the urns, sum(urns) is the total number of balls
# n     number of balls to be drawn
# 
# Returns integer vector of same length as urns. The sum of its values equals n
#' @importFrom stats rbinom rhyper
rmvhyper1draw <- function(urns, n){
  stopifnot(n>=0, n<=sum(urns))
  rest <- c(rev(cumsum(rev(urns)))[-1], 0L)
  i <- 1L
  out <- integer(length(urns))
  if (n == 0) return(out)
  for (i in seq_along(urns)){
    Ni <- urns[[i]]+rest[[i]]
    if (Ni/n > 100){
      out[i] <- rbinom(1, size = n, prob = urns[[i]]/Ni)
    } else {  
      out[i] <- rhyper(1, urns[[i]], rest[[i]], n)
    }
    n <- n-out[i]
    if (n==0) break
    i <- i+1
  }
  out
}
if (F){
  sizes <- 1:10
  data.frame(sizes, n = rmvhyper1draw(sizes, 10))
  stopifnot(rmvhyper1draw(sizes, sum(sizes)) == sizes)
}

# ------------------------------------------------------------------------------

# ==== Sample pairs _within_ groups ====

# Integers in output refer to position in sorted data, as in groupVar
samplePairsWithin <- function(grpSizes, nmax = 5000){
  n <- sum(grpSizes)
  groupVar <- rep(seq_along(grpSizes), grpSizes)
  nGroups <- length(grpSizes)
  Npairs <- choose(grpSizes, 2)
  sampleSizeTotal <- min(sum(Npairs), nmax)
  # sample sizes within groups
  sampleSizeGrps <- tabulate(sample(rep(seq_along(grpSizes), Npairs), sampleSizeTotal), nbins = nGroups)
  # draw pairs within groups
  gr <- rep(seq_len(nGroups), sampleSizeGrps)
  np <- unlist(mapply(sample, Npairs, sampleSizeGrps, SIMPLIFY = FALSE, USE.NAMES = FALSE))
  gS <- rep(grpSizes, sampleSizeGrps)
  draw <- data.frame(gr, t(mapply(findPair, np, gS)))
  names(draw) <- c("g", "i1", "i2")
  # Adjust i1 and i2 such that they match positions in sorted data
  gStarts <- c(0L, cumsum(grpSizes[-length(grpSizes)]))
  add <- gStarts[draw$g]
  draw$i1 <- draw$i1 + add
  draw$i2 <- draw$i2 + add
  draw$g <- NULL
  # check correctness of result
  chkGrps <- lapply(draw, function(x) groupVar[x])
  stopifnot(with(draw, c(c(i1, i2) %in% seq_len(n), i1<i2)),
            chkGrps[[1]] == chkGrps[[2]])
  # sort and return
  draw <- draw[with(draw, order(i1, i2)), ]
  draw
}

# ==== Sample pairs _across_ groups ====

# Integers in output refer to position in sorted data, as in groupVar
samplePairsBetween <- function(grpSizes, nmax = 5000){
  grpSizes <- as.numeric(grpSizes)
  n <- sum(grpSizes)
  groupVar <- rep(seq_along(grpSizes), grpSizes)
  cumGrpSizes <- cumsum(grpSizes)
  gRest <- c(rev(cumsum(rev(grpSizes)))[-1], 0L)
  sampleSizeTotal <- min(sum(grpSizes*gRest), nmax)
  # Prepare output data.frame
  draw <- data.frame(row.names = seq_len(sampleSizeTotal))
  # Draw first member of pairs
  sampleSizeGrps1 <- rmvhyper1draw(grpSizes*gRest, sampleSizeTotal)
  # Draw second member
  i1Freqs <- unlist(mapply(
    function(r, s, f) rmvhyper1draw(rep(r, s), f),
    gRest, grpSizes, sampleSizeGrps1,
    SIMPLIFY = FALSE))
  draw$i1 <- rep(seq_len(n), i1Freqs)
  # Adjust i2 such that it matches positions in sorted data
  w <- i1Freqs>0
  draw$i2 <- unlist(mapply(sample, gRest[groupVar][w], i1Freqs[w], SIMPLIFY = FALSE))
  add <- seq_len(n)
  add <- rep(cumGrpSizes, grpSizes)
  draw$i2 <- draw$i2 + add[draw$i1]
  # check correctness of result
  chk2 <- lapply(draw, function(x) groupVar[x])
  stopifnot(
    with(draw, c(c(i1, i2) %in% seq_len(n)), i1<i2),
    chk2[[1]] != chk2[[2]])  
  # sort and return
  # sort and return
  draw <- draw[with(draw, order(i1, i2)), ]
  draw
}

# ------------------------------------------------------------------------------

if (FALSE){ # Tests / examples

  # very small
  samplePairsWithin(1:4)
  samplePairsBetween(1:4)
  
  # small
  ng <- 10
  grpSizes <- rpois(ng, 6)+1L
  out1 <- samplePairsWithin(grpSizes)
  str(out1)
  out2 <- samplePairsBetween(grpSizes)
  str(out2)
  plot(out1)
  plot(out2)
  
  # bigger
  grpSizes <- rep(1000, 5)
  sum(grpSizes)
  system.time(out1 <- samplePairsWithin(grpSizes))
  str(out1)
  system.time(out2 <- samplePairsBetween(grpSizes))
  str(out2)
  anyDuplicated(out2$i1)
  plot(out1, pch = ".")
  plot(out2, pch = ".")
  
  # very big
  ng <- 100000
  grpSizes <- sapply(rexp(ng, rate = .05), rpois, n = 1)+1
  sum(grpSizes)
  system.time(out1 <- samplePairsWithin(grpSizes))
  str(out1)
  system.time(out2 <- samplePairsBetween(grpSizes))
  str(out2)
  anyDuplicated(out2$i1)
  plot(out1, pch = ".")
  plot(out2, pch = ".")
  
}

# ------------------------------------------------------------------------------

# utilities for a quick check of identity of input data
#' @importFrom stats setNames
recordSample <- function(x){
  x <- as.data.frame(x)
  n <- nrow(x)
  len <- 10
  indices <- setNames(replicate(ncol(x), sample(len, min(n, len)), simplify = FALSE), 
                      names(x))
  samples <- mapply("[", x, indices, SIMPLIFY = FALSE)
  list(dim = dim(x), indices = indices, samples = samples)
}

checkSample <- function(x, target){
  x <- as.data.frame(x)
  dim.ok <- identical(dim(x), target$dim)
  chk <- mapply("[", x, target$indices, SIMPLIFY = FALSE)
  samples.ok <- mapply(all.equal, target$samples, chk, SIMPLIFY = TRUE)
  c(dim = dim.ok, samples.ok)
}

if (FALSE){
  x <- iris
  target <- recordSample(iris)
  checkSample(iris, target)
}  
  
