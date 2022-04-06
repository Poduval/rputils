
#' Compute samples of dissimilarities 
#' 
#' Takes two random samples of dissimilarities between pairs of observations:
#' a sample of pairs of observations from the same \dQuote{\code{exact}-group}, 
#' and a second sample of pairs from different \dQuote{\code{exact}-group}.
#' The groups are thereby defined as sets of observations having identical values in all \code{exact} variables.
#' 
#' @details
#' Note that this does not work if there are no \code{exact} variables.
#' 
#' @param x Input object of respective class.
#' @param fuzzy,exact,standardize As in \code{\link{find_duplicates}}.
#' @param nmax The (maximal) number of pairs in each of the two samples.
#' @param \dots Currently not used.
#' 
#' @return A \code{data.frame} with additional class attribute \dQuote{dissimilarities}.
#' 
#' @seealso \code{\link{plot.dissimilarities}}, \code{\link{dissimilar}}, \code{\link{find_duplicates}}.
#' 
#' @examples 
#' # See the example in ?find_duplicates
#' 
#' @export
dissimilarities <- function (x, ...) 
  UseMethod("dissimilarities")

#' @describeIn dissimilarities Method for \code{data.frame}s.
#' 
#' @export
dissimilarities.data.frame <- function(x, fuzzy = NULL, exact = NULL, 
                                       standardize = FALSE, nmax = 5000, ...){
  # 'exact' groups
  grps <- do.call(paste, c(x[exact], list(sep = "\r")))
  tbl <- table(grps)
  grpnames <- names(tbl)
  # record sample of values
  .smpl <- recordSample(x[c(fuzzy, exact)])
  # ordering of data
  ordering <- order(grps)
  x <- x[fuzzy][ordering, ]
  # standardize if required
  if (standardize) x <- scale(x)
  # Collect sample of pairs
  pairsW <- samplePairsWithin(tbl, nmax = nmax)
  distW <- sqrt(rowMeans((x[pairsW$i1, ] - x[pairsW$i2, ])^2, na.rm = TRUE) * ncol(x))
  pairsB <- samplePairsBetween(tbl, nmax = nmax)
  distB <- sqrt(rowMeans((x[pairsB$i1, ] - x[pairsB$i2, ])^2, na.rm = TRUE) * ncol(x))
  # factor distinguishing pairs inside and across clusters
  typeLvls <- paste0("Distances ",  c("_inside_", "_across_"), " groups of {",
                     paste(exact, collapse = ", "), "}")
  # output object
  out <- data.frame(
    type = factor(rep(typeLvls, c(length(distW), length(distB))),
                  levels = typeLvls),
    dist = c(distW, distB),
    i1 = ordering[c(pairsW$i1, pairsB$i1)],
    i2 = ordering[c(pairsW$i2, pairsB$i2)])
  class(out) <- c("dissimilarities", "data.frame")
  attr(out, "sample") <- .smpl
  out
}

#' @describeIn dissimilarities Method for a \sQuote{Fuzzydedup} object \code{x}: Uses \code{fuzzy}, \code{exact}, \code{standardize} as specified in \code{x}.
#' 
#' @export
dissimilarities.Fuzzydedup <- function(x, nmax = 5000, ...){
  cl <- attr(x, "call")
  env <- attr(x, "environment")
  
  dat <- as.data.frame(eval(cl$dat, env))
  exact <- eval(cl$exact, env)
  fuzzy <- eval(cl$fuzzy, env)
  tol <- eval(cl$tol, env)
  if (is.null(tol)) tol <- formals(find_duplicates)$tol
  standardize <- eval(cl$standardize, env)
  if (is.null(standardize)) standardize <- formals(find_duplicates)$standardize
  out <- dissimilarities(dat, fuzzy = fuzzy, exact = exact, 
                         standardize = standardize, nmax = 5000, ...)
  attr(out, "tol") <- tol
  out
}


#' Draw histograms of the two samples of dissimilarities recorded in a \dQuote{dissimilarities} object.
#' 
#' @details Typically, one expects a peak close to 0 in the histogram of pairs \emph{within} groups 
#' that is not present in the histogram of of pairs \emph{between} groups. 
#' This peak is likely to correspond to pairs of \dQuote{fuzzy duplicates}.
#' 
#' @param x A \code{\link{dissimilarities}} object.
#' @param vline Position of an optional vertical cutoff line.
#' @param bins,\dots Passed to \code{geom_hisogram}.
#' 
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap xlab geom_vline
#' @importFrom IAZIgraph IAZIdarkblue IAZIlightblue IAZIred
#' @export
plot.dissimilarities <- function(x, vline = attr(x, "tol"), bins = 60, ...){
  out <- ggplot(x, aes(dist)) +
    geom_histogram(col = IAZIdarkblue, fill = IAZIlightblue, bins = bins, ...) +
    facet_wrap(~ type) +
    xlab("distance")
  if (!is.null(vline))
    out <- out + geom_vline(xintercept = vline, col = IAZIred, lwd = 1)
  out
}


if (FALSE){ # Tests / examples
  library(Fuzzydedup)

  x <- find_duplicates(iris, fuzzy = names(iris)[1:4], exact = "Species",
                       tol = 0.5, add = TRUE)
  plot(x)
  plot(x, vline = 0.2)
  require(ggplot2)
  plot(x) + xlim(0, 1)

  distances <- dissimilarities(x)
  head(distances)
  sqrt(sum((iris[1, 1:4] - iris[2, 1:4])^2))
  
}
