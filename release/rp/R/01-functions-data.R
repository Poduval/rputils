#' as.rp
#' @description converting a data.frame into \code{rp} class object
#'
#' @param x data.frame
#'
#' @return An R object of class \code{rp}
#'
#' @export
#'
as_rp <- function(x) {

  # expected to be a data.frame with at least one row and one column
  stopifnot(is.data.frame(x), nrow(x) >= 1, ncol(x) >= 1)

  # types of columns
  nms_cols <- colnames(x)
  chr_vars <- nms_cols[sapply(x, class) %in% c('character')]
  fct_vars <- nms_cols[sapply(x, class) %in% c('factor')]
  num_vars <- nms_cols[sapply(x, class) %in% c('numeric')]
  int_vars <- nms_cols[sapply(x, class) %in% c('integer')]
  lgc_vars <- nms_cols[sapply(x, class) %in% c('logical')]

  # Return rp structure
  xx <- structure(list(
    df = x,
    chr_vars = chr_vars,
    fct_vars = fct_vars,
    num_vars = num_vars,
    int_vars = int_vars,
    lgc_vars = lgc_vars
  ))
  class(xx) <- "rp"
  return(xx)
}

#' print.rp
#' @description print method for class \code{rp}
#'
#' @param x A rp class object
#' @param n number of records to display
#'
#' @return prints the data
#'
#' @examples
#' as_rp(iris)
#'
#' @export
#'
#' @importFrom utils head tail
#'
print.rp <- function(x, n = 4) {
  df <- x$df
  top <- head(df)
  bottom <- tail(df)
  cat(sprintf("top %i rows: \n", n))
  print(top)
  cat("\n")
  cat(sprintf("bottom %i rows: \n", n))
  print(bottom)
}

#' summary.rp
#' @description summary method for class \code{rp}
#'
#' @param x A rp class object
#' @param round_to decimal rounding up to
#'
#' @return summary of the data
#'
#' @examples
#' summary(as_rp(iris))
#' summary(as_rp(CO2))
#' @export
#'
#' @importFrom stats setNames
#'
summary.rp <- function(x, round_to = 2) {

  df <- x$df
  output <- list()

  # handling numeric and integer columns
  desc_vars <- c(x$num_vars, x$int_vars)
  if(length(desc_vars)) {
    xx <- df[desc_vars]
    output[["desc"]] <- do.call(rbind, apply(xx, 2, rp_desc, round_to = round_to))
  }

  # handling characters
  if(length(x$chr_vars)) {
    xx <- df[x$chr_vars]
    output[["len"]] <- apply(xx, 2, length)
  }

  # handling factors & logical
  tab_vars <- c(x$fct_vars, x$lgc_vars)
  if(length(tab_vars)) {
    xx <- df[tab_vars]
    yy <- lapply(tab_vars, function(x) {
      dd <- setNames(data.frame(table(xx[x])), c("level", "n"))
      dd
      })
    names(yy) <- tab_vars
    output[["tables"]] <- yy
  }
  return(output)
}
