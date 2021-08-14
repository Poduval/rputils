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
  structure(
    list(df = x, chr_vars = chr_vars, fct_vars = fct_vars,
         num_vars = num_vars, int_vars = int_vars,
         lgc_vars = lgc_vars),
    class = "rp")
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
print.rp <- function(x, n = 4, top = TRUE, bottom = TRUE) {
  df <- x$df
  df.top <- head(df)
  df.bottom <- tail(df)
  if (top) {
    cat(sprintf("top %i rows: \n", n))
    print(df.top)
    cat("\n")
  }
  if (bottom) {
    cat(sprintf("bottom %i rows: \n", n))
    print(df.bottom)
    cat("\n")
  }
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
summary.rp <- function(x, round_to = 2, fct_threshold = 0.3) {

  # Initiate
  df <- x$df
  nr <- nrow(df)
  output <- list()

  # Setup different variables for analysis ====
  desc_vars <- c(x$num_vars, x$int_vars)
  chr_vars <- x$chr_vars
  tab_vars <- c(x$fct_vars, x$lgc_vars)
  checkForFactors <- apply(
    df[chr_vars], 2,
    function(x) length(unique(x)))/nr <= fct_threshold

  if(any(checkForFactors)) {
    tab_vars <- c(tab_vars, chr_vars[checkForFactors])
    chr_vars <- chr_vars[chr_vars[!checkForFactors]]
  }

  # Handling numeric and integer columns ====
  if(length(desc_vars)) {
    output[["descriptives"]] <- do.call(
      rbind, apply(df[desc_vars], 2, rp_desc, round_to = round_to))
  }

  # Handling characters ====
  if(length(chr_vars)) {
    output[["chrLength"]] <- apply(
      df[chr_vars], 2, function(x) length(!is.na(x)))
  }

  # Handling factors & logical ====
  if(length(tab_vars)) {
    yy <- lapply(tab_vars, function(x) {
      setNames(data.frame(table(df[tab_vars][x])), c("level", "n"))})
    names(yy) <- tab_vars
    output[["tables"]] <- yy
  }
  # Final output ====
  structure(output, class = "rp_summary")
}

#' print.rp_summary
#' @description print method for class \code{rp_summary}
#'
#' @param x A rp_summary class object
#' @param full default FALSE, prints only descriptive statatics,
#'             if TRUE prints all output
#' @return prints summary statistics
#'
#' @examples
#' print(summary(as_rp(iris)))
#' print(summary(as_rp(iris)), T)
#' @export
#'
#' @importFrom utils head tail
#'
print.rp_summary <- function(x, full = FALSE) {
  if(full) print.default(x) else print.data.frame(x$descriptives)
}

