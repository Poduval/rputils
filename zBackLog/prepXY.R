
#' Prepare x and y variables for \code{report}
#' 
#' @description
#' These auxiliary functions are used in \code{\link{report}()} for preprocessing of the \code{x} and \code{y} variables.
#' 
#' @details
#' \code{x} can be either numeric or a factor and \code{.prepX()} always returns a factor.
#' If \code{x} is numeric, it is discretized into \code{n_groups} groups, 10 by default. 
#' Alternatively, the \code{x_breaks} can be specified explicitely. 
#' The grouping can be suppressed by setting \code{group_x=FALSE} - 
#' in this case each input value is treated as a category of its own.
#' 
#' \code{y} can be either a numeric variable or a factor and returns a factor having levels 
#' \code{"low"}, \code{"inSample"}, \code{"high"} and possibly \code{"NA"}. 
#' These categories are created according to the two cutpoint in \code{y_breaks}. 
#' If \code{y_breaks} is not specified, (rounded) 5\% and 95\% quantiles of \code{y} are used.
#' If \code{y} is a factor, it must already have levels \code{"low"}, \code{"inSample"}, \code{"high"}
#' and possibly \code{"NA"}. 
#' Note that in this case, output 2 of \code{\link{report}} looks different (see examples in \code{?\link{report}}) 
#' and output 5 is not produced.
#' 
#' Values equal to a cutpoint (\code{x_breaks} or \code{y_breaks}) are attributed to the \emph{higher} category.
#' 
#' @param x \code{x} variable.
#' @param x_breaks Numeric vector. Should usually lie within the range of \code{x}.
#' @param group_x Logical: Make groups of \code{x}?
#' @param n_groups Number of x-groups.
#' @param y \code{y} variable.
#' @param y_breaks Numeric vector of length 2: Cutpoints defining the categories \code{"low"}, \code{"inSample"}, \code{"high"}. 
#' Should usually lie within the range of \code{y}.
#' 
#' @seealso \code{\link{report}}
#' 
#' @examples
#' x <- exp(rnorm(100))
#' 
#' .prepX(x)
#' .prepX(x, n_groups = 5)
#' .prepX(x, x_breaks = c(1, 3, 7))
#' .prepX(x, group_x = FALSE)
#' x_na <- ifelse(runif(100)<.9, x, NA)
#' .prepX(x_na)
#' 
#' .prepY(x)
#' .prepY(x, y_breaks = c(1, 4))
#' y_na <- ifelse(runif(100)<.9, x, NA)
#' .prepY(y_na)
#' 
#' @name prepXY
NULL

#' @rdname prepXY
#' @export
.prepX <- function(x, group_x = is.numeric(x), x_breaks = NULL, n_groups = 10){
  if (!group_x){
    out <- factor(x)
  } else {
    spread_x <- IQR(x[is.finite(x)], na.rm = TRUE)
    if (spread_x == 0) spread_x <- diff(range(x, na.rm = TRUE))
    if (spread_x == 0) stop("x variable is constant")
    s <- 10^ceiling(log10(spread_x / 50)) ## 10^ceiling(log10(diff(range(x)) / 100))
    if (is.null(x_breaks)){
      x_breaks <- as.vector(quantile(x[is.finite(x)], seq_len(n_groups-1)/(n_groups), na.rm = TRUE))
      x_breaks <- round(x_breaks, -log10(s))
    } else x_breaks <- sort(x_breaks)
    if (min(x, na.rm = TRUE) < x_breaks[[1]])
    	x_breaks <- c(floor(min(x, na.rm = TRUE)/s - 1e-12) * s, 
                    x_breaks)
    if (max(x, na.rm = TRUE) > x_breaks[[length(x_breaks)]])
    	x_breaks <- c(x_breaks,
    								ceiling(max(x, na.rm = TRUE)/s) * s)
    x_breaks <- unique(x_breaks)
    lvls <- paste0(format(x_breaks[-length(x_breaks)], justify = "r"), " - ", format(x_breaks[-1], justify = "r"), " ")
    out <- cut(x, x_breaks, labels = lvls)
  }
  if (any(is.na(out))){
    lvls <- levels(out)
    out <- as.character(out)
    out[is.na(out)] <- "NA"
    out <- factor(out, levels = c(lvls, "NA"))
  }
  out
}

#' @rdname prepXY
#' @export
.prepY <- function(y, y_breaks = NULL){
	if (is.character(y)) y <- factor(y)
  hasna <- anyNA(y) || (is.factor(y) && "NA" %in% levels(y))
  lvls <- c("low", "inSample", "high", if (hasna) "NA")
  if (is.factor(y)){
    stopifnot(levels(y) %in% lvls)
  	if (any(is.na(y))){
  		y <- as.character(y)
  		y[is.na(y)] <- "NA"
  	}
    out <- factor(y, levels = lvls)
  } else {
    if (is.null(y_breaks)){
      y_breaks <- as.vector(quantile(y[is.finite(y)], c(0.05, 0.95), na.rm = TRUE))
      s <- floor(log10(IQR(y[is.finite(y)], na.rm = TRUE) / 50))
      y_breaks <- round(y_breaks, -s)
    }
    stopifnot(length(y_breaks) == 2)
    out <- factor(rep(NA, length(y)), levels = lvls)
    out[y < y_breaks[1]] <- "low"
    out[is.na(out) & y < y_breaks[2]] <- "inSample"
    out[is.na(out) & y >= y_breaks[2]] <- "high"
    if (hasna) out[is.na(y)] <- "NA"
  }
  structure(out, y_breaks = y_breaks)
}
