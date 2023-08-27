
#' Determine and display variable importance
#' 
#' These functions are mostly wrappers to corresponding functions in other packages.
#' 
#' @name importance
#' 
#' @param x An input model.
#' @param sorted Logical: Sort importance values (in decreasing order)?
#' @param relative Logical: Scale importance values relative to largest importance value?
#' @param \dots Argments passed to methods.
#' 
#' @seealso For examples, see \code{\link{gDiff-package}}.
#' @name importance
NULL

#' @rdname importance
#' @export
importance <- function(x, ...) UseMethod("importance")
#' @rdname importance
#' @export
importance.fittedgDiff_xgboost <- function(x, ...){
  imp <- xgb.importance(x$fit$feature_names, model = x$fit, ...)
  out <- imp[, 2, drop = TRUE]
  names(out) <- imp[, 1, drop = TRUE]
  structure(out, class = c("importance_fittedgDiff", "numeric"))
}

#' importFrom ranger importance importance.ranger
#' @rdname importance
#' @export
importance.fittedgDiff_ranger <- function(x, ...){
  imp <- ranger::importance(x$fit)
  structure(imp, class = c("importance_fittedgDiff", "numeric"))
}
#' @rdname importance
#' @export
plot.importance_fittedgDiff <- function(x, relative = TRUE, sorted = TRUE, ...){
	if (relative) x[] <- x / max(x)
	if (sorted) x <- sort(x)
	ggplot(data.frame(variable = factor(names(x), levels = names(x)), 
										importance = x), 
				 aes(variable, importance)) + 
		geom_bar_1(stat = "identity") +
		coord_flip()
}
	
#' @rdname importance
#' @export
importance.lm <- function(x, ...){
	abs(getStdBeta(x, ...))
}

# Extract standardized beta coefficients from a lm
# [https://www.researchgate.net/profile/Jeff_Johnson/publication/215519832_History_and_Use_of_Relative_Importance_Indices_in_Organizational_Research/links/0d1c84f4531ebe2550000000.pdf]
#' @importFrom matrixStats colSds
getStdBeta <- function(object, ...){
	prdTrms <- predict(object, type = "terms")
  out <- colSds(prdTrms) / getsdy(object)
  names(out) <- colnames(prdTrms)
  out
}
# Extract sd(Y) from a lm object
getsdy <- function(object){
	if (inherits(object, "glm")) return(1)
	smry <- summary(object)
	dendf <- smry$fstatistic[["dendf"]]
	numdf <- smry$fstatistic[["numdf"]]
	smry$sigma / sqrt((1-smry$r.squared) * (numdf + dendf) / dendf)
}
# Examples/checks of getsdy & getStdBeta
if (F){
	a <- lm(Sepal.Length ~ ., iris, model = F)
	getsdy(a)  
	sd(iris$Sepal.Length)
	getStdBeta(a)
	d <- cbind(as.data.frame(model.matrix(a))[-1], iris["Sepal.Length"])
	b <- lm(Sepal.Length ~ ., d, model = T)
	QuantPsyc::lm.beta(b)
}

#' @rdname importance
#' @export
importance.fittedgDiff_lm <- function(x, ...){
  structure(importance(x$fit), 
  					class = c("importance_fittedgDiff", "numeric"))
}
#' @rdname importance
#' @export
importance.fittedgDiff_glm <- importance.fittedgDiff_lm

