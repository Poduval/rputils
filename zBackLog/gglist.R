
#' Methods for the class \dQuote{gglist}
#' 
#' @description 
#' The output of \code{\link{diagPlot}()} is a list of ggplots with class \dQuote{gglist}.
#' (Nowadays, one would probably use \code{ggpubr::ggarrange}).
#' 
#' In the \code{plot} method, you can choose the layout of the panels with \code{nrow} and \code{ncol}.
#' Multi-page layouts, are also possible. See examples.
#' 
#' @param object,x Object of class \dQuote{gglist}, a list of ggplots.
#' @param \dots Not used
#' @param i Selection of plots (integer or logical)
#' @param nrow,ncol Integers: Defines layout of plots on a page.
#' @param ask Logical, only relevant for multi-page layouts: 
#' Wait for user confirmation (enter key) before switching to next page?
#' 
#' @examples
#' # Model with simulated data having outliers
#' n <- 1000
#' x <- rnorm(n)
#' y <- x + rt(n, 3)
#' d <- data.frame(x, y)
#' rm(x, y, n)
#' plot(y~x, d)
#' 
#' # Refit roblm model as weighted lm()
#' d$w <- robustbase::lmrob(y~x, d)$rweights
#' weighted_model <- lm(y~x, d, weights = w)
#' 
#' # Diagnostics plots
#' dp <- diagPlot(weighted_model)
#' dp  #same as plot(dp)
#' str(dp)
#' 
#' # Modify layout
#' plot(dp, ncol = 2, nrow = 2, ask = TRUE)
#' 
#' # Plot a subset of the panels
#' dp[c(2, 4)]
#' 
#' @name gglist
NULL

#' @describeIn gglist \code{str} method
#' @importFrom utils str
#' @export
str.gglist <- function(object, ...){
  cl <- deparse(attr(object, "call"))
  if (length(cl)>1) cl <- paste0(cl[[1]], "...")
  cat("Class 'gglist'\n", "Call:  ", cl, "\n", sep = "")
  nms <- names(object)
  if (is.null(nms)) nms <- rep("[unnamed plot]", length(object))
  titles <- vapply(object, gettitle, character(1), USE.NAMES = FALSE)
  if (any(is.na(titles))) titles[is.na(titles)] <- "[untitled element]"
  writeLines(paste0("  ", format(paste0(nms, ": ")), 
                    titles))
  invisible()
}
# Auxiliary function: extraacts the plot title from a ggplot object
gettitle <- function(x){
  lbl <- x$labels$title
  if (is.null(lbl)) lbl <- NA_character_
  lbl 
}

#' @describeIn gglist Subset a \dQuote{gglist} object
#' @export
`[.gglist` <- function(x, i){
  out <- NextMethod()
  structure(out, 
            call = match.call(), 
            class = "gglist")
}

#' @describeIn gglist Plot a \dQuote{gglist} object
#' 
#' @importFrom graphics plot par
#' @importFrom gridExtra arrangeGrob
#' @export
plot.gglist <- function(x, 
                        nrow = sum(length(x) > c(0, 1, 4)), 
                        ncol = min(ceiling(length(x)/nrow), 3), 
                        ask = par("ask"), 
                        ...){
  page <- (seq_along(x)-1) %/% (ncol*nrow) + 1
  xspl <- split(x, page)
  out <- lapply(xspl, function(g) 
     arrangeGrob(grobs = g, nrow = nrow, ncol = ncol, ...))
  if (length(out) > 1 && !missing(ask)){
    opar <- par(ask = ask)
    on.exit(par(opar))
  }
  lapply(out, plot)
  invisible(out)
}

#' @describeIn gglist Same as the \code{plot} method.
#' @export
print.gglist <- plot.gglist

