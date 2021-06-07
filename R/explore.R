#' explore method
#' @param x vector/data.frame
#' @return The sum of \code{x} and \code{y}
#' @examples
#' explore(iris)
#' explore(iris$Sepal.Length)
#' explore(iris, colNames = c('Sepal.Length', 'Petal.Length', 'Species'))
#' @export
#'
explore <- function(x, ...) {
  UseMethod("explore")
}

#' explore.default
#' @param x an input vector
#' @param rnd rounding off the results
#'
#' @export FALSE
explore.default <- function(x, rnd = 4, ...) {

  # quick check: input must be numeric
  stopifnot(class(x) %in% c('integer', 'numeric'))

  # defining the quantiles for the function
  prbs <- c(0, 0.05, 0.25, 0.50, 0.75, 0.95, 1)
  nams <- c("Min", "P05", "P25", "Med", "P75", "P95", "Max")
  qnts <- rbind.data.frame(round(quantile(x, probs = prbs, na.rm = T, names = F), rnd))
  names(qnts) <- nams

  # returning output as a data.frame
  cbind.data.frame(
    N = length(x), Mis = sum(is.na(x) | x < 0),
    Avg = round(mean(x, na.rm = T), rnd),
    qnts, Std = round(sd(x, na.rm = T), rnd))
}

#' explore.data.frame
#' @param x a data frame
#' @param rnd rounding off the results
#' @param colNames vector of column names of the input data.
#'
#' @export FALSE
explore.data.frame <- function(x, rnd = 4, colNames = colnames(x), ...) {

  # quick check: input data.frame must have 1 row
  stopifnot(nrow(x) > 0, colNames %in% colnames(x))

  # excluding non numeric columns from the analysis
  c <- colnames(x[!sapply(x, class) %in% c('character', 'factor')])
  c <- intersect(c, colNames)

  # confirming existence of numeric columns
  stopifnot(length(c) > 0)

  # applying the default function to each numeric columns of a data.frame
  if (length(c) == 1) { # special case for a single column
    out <- explore.default(x[, c], rnd = rnd)
    rownames(out) <- c
  } else { # when there are multiple columns to analyse
    out <- do.call(rbind, apply(x[c], 2, explore.default, rnd = rnd))
  }
  return(out)
}
