
#' Comparison of two groups 
#' 
#' \code{gDiff} is a simple generator function specifying an object of the corresponding class.
#' 
#' @param data A \code{data.frame} or someting that \code{as.data.frame} can handle.
#' May contain \code{NA}'s.
#' @param g A vector of length \code{nrow(data)} containing exactly two distinct values.
#' \code{NA}'s are not allowed.
#' @param \dots Argments passed to methods.
#' 
#' @return A list of \code{data} (converted to a \code{\link{data.frame}}) and 
#' \code{g} (converted to a \code{\link{factor}}) with class attribute 'gDiff'.
#' 
#' @seealso \code{\link{fit.gDiff}}. For examples, see \code{\link{gDiff-package}}.
#' @importFrom matrixStats colAlls
#' @export
gDiff <- function(data, g){
  if (any(is.na(g))) stop("NA's not allowed in g")
  stopifnot(NROW(data) > 0, NCOL(data) > 0, NROW(data) == length(g),
            length(unique(g)) == 2L, !anyNA(g))
  data <- as.data.frame(data)
  if (any(isChar <- sapply(data, is.character))){
    data[isChar] <- lapply(data[isChar], factor)
  }
  # Columns without valid data
  nas <- is.na(data)
  dropCol <- colAlls(nas)
  if (any(dropCol)){
    warning("Column(s) with only NA's dropped: ", paste(names(data)[dropCol], collapse = ","))
    data <- data[, !dropCol, drop = FALSE]
  }
  structure(list(
    data = data, g = factor(g)),
    class = "gDiff")
}
#' @describeIn gDiff
#' Print a short summary of the contents of \code{x}.
#' @param width Approximate width of the printed output.
#' @export
print.gDiff <- function(x, ..., width = 60){
  cat("--- Class 'gDiff' ---\n")
  tbl <- table(x$g)
  cat(paste0(c("Groups: ", "        "),
             format(names(tbl)), " (n=", tbl, ")"),
      sep = "\n")
  p <- length(x$data)
  cat("\nVariables (", p, "):\n", sep = "")
  cat(names(x$data), sep = ", ", fill = width, labels = " ")
  # Factor/character variables
  if (any(fac <- sapply(x$data, is.factor))){
    cat("\nFactors (with no. of levels):\n")
    which_fac <- which(fac)
    facs <- paste0(names(x$data)[which_fac], "(", sapply(x$data[which_fac], nlevels), ")")
    do.call(cat, list(facs, fill = width, labels = " ", sep = ", "))
    }
  # Variables with NA's
  if (any(nas <- is.na(x$data))){
    p.na <- colMeans(nas)
    nalimits <- c(-1, 0, 1, 10, 50, 100)
    naclass <- cut(p.na*100, nalimits)
    cat("\nVariables with NA's:\n")
    for (i in 2:(length(nalimits)-1)){
      if (any(as.integer(naclass) == i)){
        cat(" ", nalimits[i], " to ", nalimits[i+1], "% of NA's:\n", sep = "")
        cat(paste0(names(x$data)[as.integer(naclass) == i], collapse = ", "), 
            fill = width, labels = "   ", sep = "")
      }
    }
  }

  invisible(x)
}

#' @describeIn gDiff
#' Display the distribution of a variables (or expressions of variables) in the two groups of a 'gDiff'.
#' 
#' @param x A \code{\link{gDiff}} object.
#' @param formula A one- or two-sided \code{\link{formula}} 
#' (e.g. \code{~x} or \code{~x+y^2}, or \code{y~x}) that can be evaluated in \code{x$data}.
#' @param discrete Logical: Display a separate bar for each unique value or group values into bins?
#' @param discrete.x,discrete.y Separate specification of discreteness for \code{x} and \code{y} 
#' axis variable in case of a two-sided formula.
#' 
#' @import IAZIgraph
#' @import ggplot2
#' @export
plot.gDiff <- function(x, formula, discrete = NULL, discrete.x = discrete, discrete.y = discrete, ...){
  if (missing(formula)) stop("'formula' argument is required.")
  stopifnot(inherits(formula, "formula"), length(formula) %in% 2:3)
  bivar <- length(formula) == 3
  xvar.pos <- if (bivar) 3 else 2
  
  # define plot-data and 'discrete' flags
  d <- data.frame(row.names = rownames(x$data))
  d$g <- x$g
  d$x <- eval(formula[[xvar.pos]], x$data, parent.frame())
  if (is.null(discrete.x)) discrete.x <- is.character(d$x) || is.factor(d$x) || n_distinct(d$x) <= 20
  if (discrete.x && is.numeric(d$x) && !is.factor(d$x))
    d$x <- factor(d$x, levels = as.character(sort(unique(d$x))))
  if (bivar){
    d$y <- eval(formula[[2]], x$data, parent.frame())
    if (is.null(discrete.y)) discrete.y <- is.character(d$y) || is.factor(d$y) || n_distinct(d$y) <= 20
    if (discrete.y && is.numeric(d$y) && !is.factor(d$y))
      d$y <- factor(d$y, levels = as.character(sort(unique(d$y))))
  }
  # Univariate case
  if (!bivar){
    out <- 
      ggplot(d, aes(x, fill = "group")) + 
        (if (discrete.x) geom_bar_1(...) else geom_histogram_1(...)) +
        facet_wrap(~g, scales = "free_y", ncol = 1) +
        xlab(formula[[2]])
    return(out)
  }
  if (!discrete.y){
    out <- 
      ggplot(d, aes_string("x", "y")) + 
        geom_hex_1(...) + facet_wrap(~g) +
        xlab(formula[[3]]) + ylab(formula[[2]])
    return(out)
  }
  if (!discrete.x){
    out <- 
      ggplot(d, aes_string("y", "x")) + 
        geom_hex_1(...) + facet_wrap(~g) +
        xlab(formula[[2]]) + ylab(formula[[3]])
    return(out)
  }
  d <- subset(as.data.frame(table(d)), Freq>0)
  ggplot(d, aes_string("x", "y", fill = "Freq")) +
    geom_tile() + facet_wrap(~g) +
    scale_fill_gradientn(colours = c(IAZIlightgreen, IAZIgreen, IAZIblue, IAZIdarkblue)) +
    xlab(formula[[3]]) + ylab(formula[[2]])
}
