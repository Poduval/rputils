
#' Assessment the quality of separation
#' 
#' @name separation
#' 
#' @description 
#' \code{separation} is applied to a \dQuote{fittedgDiff} object and returns a \code{data.frame} 
#' with additional class \dQuote{separation}. 
#' The rows in this data frame correspond to those in the original data, and the columns are:
#' \itemize{
#' \item{\code{group}: Group membership with respect to the two-level grouping factor.} 
#' \item{\code{sample}: Role in model training process: \code{"train"} or \code{"test"}.}
#' \item{\code{prediction}: The prediction scores, usually a value in \eqn{[0,1]} 
#' (but may be outside for \code{method="lm"}).}
#' }
#' 
#' Available methods for an object of class \dQuote{separation} are:
#' \itemize{
#' \item{\code{summary}: Display rates of correct/false classification by \code{group} and \code{sample}.}
#' \item{\code{plot}: Draws histograms of the distribution of \code{prediction} by \code{group} and \code{sample}.}
#' }
#' 
#' @param x,object Inputs of class \dQuote{fittedgDiff} or \dQuote{separation}, respectively.
#' @param gDiff Object of class \dQuote{gDiff} - the input used for generating \code{x}.
#' @param digits Number of digits.
#' @param \dots Argments passed to methods.
#' 
#' @seealso For examples, see \code{\link{gDiff-package}}.
NULL

#' @rdname separation
#' 
#' @export
separation <- function(x, ...) UseMethod("separation")

#' @rdname separation
#' @importFrom stats predict
#' @export
separation.fittedgDiff <- function(x, gDiff = eval.parent(xfit$call$x), ...){
  structure(x$pred, class = c("separation", "data.frame"))
}
#' @rdname separation
#' @param threshold threshold
#' @param absolute Logical
#' @importFrom stats xtabs uniroot
#' @importFrom dplyr mutate
#' @export
summary.separation <- function(object, threshold = NULL, 
                               absolute = FALSE, digits = 3, ...){
  if (is.null(threshold)){
    threshold <- getThreshold(object, ...) # dots go to uniroot()
  }
  d <- mutate(object, 
    classification = factor(levels(group)[(prediction >= threshold) + 1L],
                            levels(group)))
  tbl <- xtabs(~ group + classification + sample, d, addNA = TRUE)
  if (!absolute)
    tbl <- round(prop.table(tbl , margin = c(1, 3)), digits = digits)
  structure(tbl, class = "summary_separation", 	
  					freqType = if (absolute) "absolute" else "relative",
  					threshold = threshold)
}
getThreshold <- function(x, ...){
	subs <- subset(x, sample == "train")
	f <- function(l){
		tapp <- tapply(subs$prediction, subs$group, 
									 function(x) mean(x < l, na.rm = TRUE))
		sum(tapp) - 1
	}
	out <- try(uniroot(f, 0:1, ...)$root)
	if(inherits(out, "try-error")){
		threshold <- mean(subs$prediction, na.rm = TRUE)
		warning("Default calculation of threshold failed - threshold is set to mean prediction score in training set.")	
	}
	out
}
#' @export
print.summary_separation <- function(x, digits = 2, right = TRUE, quote = FALSE, ...){
	out <- structure(x, call = NULL, class = NULL)
	if (attr(out, "freqType") == "relative") out[] <- pctFmt(x, digits = digits)
	print.default(out, right = right, quote = quote, ...)
	invisible(out)
}
#' @rdname separation
#' @param colValues Two color values (for train and test data).
#' @export
plot.separation <- function(x, ..., colValues = c(IAZIdarkgreen, IAZIblue)){
  ggplot(x, aes_string("prediction", fill = "sample", color = "sample")) +
    geom_histogram(alpha = 0.2) + 
    facet_wrap(~ group + sample, nrow = 2, scales = "free_y") +
    geom_vline(xintercept = 0:1, lty = 3) +
    scale_fill_manual(values = colValues) + 
    scale_color_manual(values = colValues)
}

