#' as.rp
#' @description converting a data.frame into \code{rp} class object
#' @param df data.frame
#' @param fct_threshold threshold to consider a character as factor
#' @return An R object of class \code{rp}
#' @examples
#' as_rp(iris)
#' @export
#'
as_rp <- function(df, fct_threshold = 0.20) {

  # expected to be a data.frame with at least one row and one column
  stopifnot(is.data.frame(df), nrow(df) >= 1, ncol(df) >= 1)
  nr <- nrow(df)

  # types of columns ====
  nms_cols <- colnames(df)
  chr_vars <- nms_cols[sapply(df, class) %in% c('character')]
  fct_vars <- nms_cols[sapply(df, class) %in% c('factor')]
  num_vars <- nms_cols[sapply(df, class) %in% c('numeric')]
  int_vars <- nms_cols[sapply(df, class) %in% c('integer')]
  lgc_vars <- nms_cols[sapply(df, class) %in% c('logical')]

  # Check & update factors from the chr variable list ====
  checkForFactors <- apply(
    df[chr_vars], 2,
    function(x) length(unique(x)))/nr <= fct_threshold

  if(any(checkForFactors)) {
    fct_vars <- c(fct_vars, chr_vars[checkForFactors])
    chr_vars <- chr_vars[chr_vars[!checkForFactors]]
  }

  # Return rp structure ====
  structure(list(df = df),
            chr_vars = chr_vars, fct_vars = fct_vars,
            num_vars = num_vars, int_vars = int_vars,
            lgc_vars = lgc_vars,
            class = "rp",
            methods = c("print", "summary"))
}

#' print.rp
#' @description print method for class \code{rp}
#' @param x A rp class object
#' @param n number of records to display
#' @param top default is TRUE, prints the top rows
#' @param bottom default is TRUE, prints the bottom rows
#' @return prints the data
#' @examples
#' as_rp(iris)
#' @export
#' @importFrom utils head tail
#'
print.rp <- function(rp, n = 4, top = TRUE, bottom = TRUE) {
  df <- rp$df
  df.top <- head(df, n)
  df.bottom <- tail(df, n)
  if (top) {
    cat(sprintf("top %i rows: \n", n))
    cat("---------------------------------- \n")
    print.data.frame(df.top)
    cat("\n")
  }
  if (bottom) {
    cat(sprintf("bottom %i rows: \n", n))
    cat("---------------------------------- \n")
    print.data.frame(df.bottom)
  }
  cat("\n")
  cat("Additional info (attributes) : \n")
  cat("---------------------------------- \n")
  cat(str(attributes(rp)))
}

#' summary.rp
#' @description summary method for class \code{rp}
#' @param x A rp class object
#' @param round_to decimal rounding up to
#' @return summary of the data
#' @examples
#' summary(as_rp(iris))
#' summary(as_rp(CO2))
#' @export
#' @importFrom stats setNames
#'
summary.rp <- function(x, round_to = 2) {

  # Initiate
  df <- x$df
  nr <- nrow(df)
  output <- list()

  # Setup different variables for analysis ====
  desc_vars <- c(attr(x, "num_vars"), attr(x, "int_vars"))
  chr_vars <- attr(x, "chr_vars")
  tab_vars <- c(attr(x, "fct_vars"), attr(x, "lgc_vars"))

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
    df[tab_vars] <- lapply(df[tab_vars], as.factor)

    # adding length of the factor levels
    ll <- unlist(lapply(tab_vars, function(x) length(levels(df[[x]]))))
    names(ll) <- tab_vars
    output[["factorLevelLength"]] <- ll

    # adding frequency table for the factor variables
    yy <- lapply(tab_vars, function(x) {
      field_names <- c("variable", "level", "n")
      setNames(data.frame(x, table(df[tab_vars][x])), field_names)})
    output[["tables"]] <- do.call(rbind, yy)
  }
  # Final output ====
  structure(output, class = "summary.rp")
}

#' print.summary.rp
#' @description print method for class \code{summary.rp}
#' @param x A summary.rp class object
#' @param full default FALSE, prints only descriptive statatics,
#'             if TRUE prints all output
#' @return prints summary statistics
#' @examples
#' print(summary(as_rp(iris)))
#' print(summary(as_rp(CO2)))
#' @export
#' @importFrom utils head tail
#'
print.summary.rp <- function(x) {

  if(nrow(x$descriptives) > 0) {
    cat("1. Descriptives of numeric variables: \n")
    cat("------------------------------------- \n")
    print.data.frame(x$descriptives)
    cat("\n")
  } else {
    cat("No numeric variables found: \n")
    cat("------------------------------------- \n")
    cat("\n")
  }

  if (length(x$factorLevelLength)) {

    cat("2. Desctriptives of factor variables : \n")
    cat("-------------------------------------- \n")
    cat("\n")
    cat("2.1 Number of levels: \n")
    cat("------------------------------------------- \n")
    print(x$factorLevelLength)
    cat("\n")
    cat("2.2 Frequency of levels: \n")
    cat("------------------------------------- \n")
    print(x$tables)
  }
}

