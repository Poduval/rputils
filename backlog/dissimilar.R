
#' Find the maximal dissimilarity two observations in the same cluster are allowed to have.
#' 
#' The value is computed as \code{sqrt(tol*length(fuzzy))}, where
#' \code{tol} and \code{fuzzy} are the values of the corresponding parameters used in \code{x}.
#' 
#' @param x A \dQuote{Fuzzydedup} object - see \code{\link{find_duplicates}}.
#' 
#' @examples 
#' # See the example in ?find_duplicates
#' 
#' @export
getMaxDiss <- function(x){
  stopifnot(inherits(x, "Fuzzydedup"))
  sqrt(attr(x, "tol") * length(attr(x, "fuzzy")))
}

#' Output the most dissimilar pairs of observations in same cluster
#' 
#' This function is designed to assess of the appropriateness of the value of \code{tol} used in 
#' \code{\link{find_duplicates}()}.
#' 
#' Providing a \code{diss} object can significantly reduce execution time - 
#' thereby, the inputs \code{x} and \code{diss} must match (i.e. have been generated with the same 
#' data and the same values of \code{exact}, \code{fuzzy}, \code{standardize}).
#' 
#' @param x A \dQuote{Fuzzydedup} object - see \code{\link{find_duplicates}}.
#' @param diss A \code{\link{dissimilarities}} object.
#' @param n Number of pairs to record/display.
#' 
#' @examples 
#' # See the example in ?find_duplicates
#'
#' @export
dissimilar <- function(x, diss = NULL, n = 6, ...){
  # collect attributes of fuzzy deduplication
  exact <- attr(x, "exact")
  fuzzy <- attr(x, "fuzzy")
  standardize <- attr(x, "standardize")
  groupVarName <- attr(x, "groupVarName")

  # Retrieve data
  if (all(c(exact, fuzzy) %in% names(x))){
    d <- as.data.frame(x[c(exact, fuzzy)])
  } else {
    d <- with(attributes(x), eval(call$d, environment))[c(exact, fuzzy)]
  }
  # 'dissimilarities' object
  if (is.null(diss)){
    diss <- dissimilarities(x, ...)
  } else {
    # check compatibility
    stopifnot(checkSample(d[c(fuzzy, exact)], attr(diss, "sample")))
  }
  diss <- diss[c("i1", "i2", "dist")]
  # Select pairs of obs in same cluster
  sameCluster <- x[[groupVarName]][diss$i1] == x[[groupVarName]][diss$i2]
  dissIn <- diss[sameCluster, c("i1", "i2", "dist")]
  # pairs most far apart in same cluster
  selectedPairs <- head(dissIn[order(dissIn$dist, decreasing = TRUE), ], n)
  rownames(selectedPairs) <- NULL
  # check
  if (!isTRUE(all.equal(d[selectedPairs$i1, exact], d[selectedPairs$i2, exact],
                        check.attributes = FALSE))){
    stop("x and diss not compatible.")
  }
  # output
  dfuzzy <- as.matrix(d[fuzzy])
  dexact <- d[selectedPairs$i1, exact, drop = FALSE]
  out <- structure(list(
      n = n,
      maxDiss = getMaxDiss(x),
      distances = selectedPairs,
      exactValues = dexact,
      fuzzyValues = list(dfuzzy[selectedPairs$i1, ], dfuzzy[selectedPairs$i2, ])),
    class = "dissimilar")
  out
}

#' @describeIn dissimilar \code{print} method.
#' 
#' @param digits Number of digits to print.
#' @param \dots not used.
#' 
#' @importFrom utils capture.output
#' @export
print.dissimilar <- function(x, digits = getOption("digits"), n = x$n, ...){
  cat("Maximal distance allowed within clusters: ", format(x$maxDiss, digits = digits), "\n\n",
      "The ", n, " most dissimilar pairs of obs. in same cluster:\n",
      sep = "")
  w <- nchar(n)
  hspace <- paste(rep(" ", w+2), collapse = "")
  for (i in seq_len(n)){
    cat(format(i, width = w), ". Distance: ", format(x$distances$dist[[i]], digits = digits), "\n", 
        hspace, "'exact':  ", paste0(names(x$exactValues), "=", format(x$exactValues[i, ]), collapse = ", "), "\n",
        hspace, "'fuzzy':\n",
        sep = "")
    output_fuzzy <- capture.output(
      print(rbind(x$fuzzyValues[[1]][i, , drop = FALSE], x$fuzzyValues[[2]][i, , drop = FALSE]),
            digits = digits))
    cat(paste0(hspace, output_fuzzy, collapse = "\n"))
    if (i<n) cat("\n\n")
  }
  invisible(x)
}

