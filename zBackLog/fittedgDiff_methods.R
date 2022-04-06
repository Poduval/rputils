
#' Methods for class \dQuote{fittedgDiff}
#'
#' @description
#' \code{print} and \code{summary} display the contents of \code{x}.
#' 
#' \code{plot} is only available for objects \code{\link{fit}}ted with \code{method="xgboost"}.
#' It plots the evolution log of training and test error.
#' 
#' \code{predict} predicts group membership probabilities (a value in \eqn{[0,1]}, 
#' 0 corresponding to doubtless membership in the first group) for the \code{newdata} based on the model 
#' \code{object$fit}.
#' 
#' \code{\link{separation}} and \code{\link{importance}} are additional methods. 
#' They are described in separate help topics. 
#' 
#' @param x,object Object of class \dQuote{fittedgDiff} (the output of \code{\link{fit}}).
#' @param newdata Data for predictions.
#' @param digits Number of digits.
#' @param colValues Colors of the two curves.
#' @param \dots Argments passed to methods.
#' 
#' @seealso For examples, see \code{\link{gDiff-package}}.
#' 
#' @name fittedgDiff_methods
NULL

#' @rdname fittedgDiff_methods
#' @importFrom stats binom.test
#' @export
print.fittedgDiff <- function(x, digits = 2, ...){
	grpSizes <- table(x$pred$group)
	smplSizes <- table(x$pred$sample)
	deparsedCall <- deparse(x$call, width.cutoff = max(getOption("width") - 20, 20))
	sumSep <- summary(separation(x), absolute = TRUE)
	grps <- dimnames(sumSep)[[1]]
	ok <-    sumSep[grps[1], grps[1], ] + sumSep[grps[2], grps[2], ]
	notok <- sumSep[grps[1], grps[2], ] + sumSep[grps[2], grps[1], ]
	na <- if (dim(sumSep)[2] == 3) sum(sumSep[, 3, ]) else 0
	misCl <- pctFmt(1 - ok/(ok+notok), digits)
	names(misCl) <- names(ok)
	ci.misCl.test <- binom.test(notok[2], ok[2]+notok[2])$conf.int
	ci.misCl.test <- pctFmt(ci.misCl.test, digits = digits)
	ci.misCl.test <- paste0("(", paste(ci.misCl.test, collapse = ","), ")")
	cat(
		"--- 'fittedgDiff' object ---",
		"\nModel of class:         ", paste(class(x$fit), collapse = ", "), 
		"\nCall:                   ", deparsedCall[[1]], if (length(deparsedCall) > 1) "...",
		"\nGroups:                 ", paste0(names(grpSizes)[[1]], " (n=", grpSizes[[1]], ")"),
		"\n                        ", paste0(names(grpSizes)[[2]], " (n=", grpSizes[[2]], ")"),  
		"\nSamples:                ", paste0(names(smplSizes)[[1]], " (n=", smplSizes[[1]], ")"),
		"\n                        ", paste0(names(smplSizes)[[2]], " (n=", smplSizes[[2]], ")"),
		"\nMisclassification rate: ", paste0("train: ", misCl["train"]),
		"\n                        ", paste0("test:  ", misCl["test"], " ", ci.misCl.test),
		if (na>0) paste("\nNot classified:         ", pctFmt(na/sum(smplSizes), digits = digits)),
  	"\nExecution time [s]:     ", x$timing, "\n"
	)
  invisible(x)
}
# Aux fn
pctFmt <- function(x, digits = 2) sprintf(paste0("%.", digits, "f%%"), x*100)

#' @rdname fittedgDiff_methods
#' @export
predict.fittedgDiff <- function(object, newdata, ...){
	predict(object$fit, newdata, ...)
}
#' @rdname fittedgDiff_methods
#' @export
predict.fittedgDiff_ranger <- function(object, newdata, ...){
	predict(object$fit, newdata, ...)$predictions
}
#' @rdname fittedgDiff_methods
#' @export
predict.fittedgDiff_glm <- function(object, newdata, ...){
	predict(object$fit, newdata, type = "response", ...)
}
#' @rdname fittedgDiff_methods
#' @export
predict.fittedgDiff_xgboost <- function(object, newdata, ...){
	newdata <- as.data.frame(newdata)
  m <- model.matrix_na.pass(newdata, vars = object$vars)
  predict(object$fit, xgb.DMatrix(data = m))
}


#' @rdname fittedgDiff_methods
#' @export
plot.fittedgDiff <- function(x, ...){
	message("No plot method available for object of class c(", paste0('"', class(x), '"', collapse = ", "), ")")
}
#' @rdname fittedgDiff_methods
#' @export
plot.fittedgDiff_xgboost <- function(x, ..., 
																		 colValues = c(IAZIdarkgreen, IAZIblue)){
  evallog <- x$fit$evaluation_log
  plotdata <- with(evallog, data.frame(
      iter = rep(iter, 2),
      type = rep(c("train", "test"), each = nrow(evallog)),
      logloss = c(val1_logloss, val2_logloss)))
  ggplot(plotdata, aes_string("iter", "logloss", color = "type")) + 
    geom_line() + ylim(0, NA) +
    scale_color_manual(values = colValues) +
    geom_hline(yintercept = 0, col = IAZIred, lty = 2)
}

#' @rdname fittedgDiff_methods
#' @export
summary.fittedgDiff <- function(object, ...){
	print(object)
	cat("\nSeparation:\n")
	print(summary(separation(object)))
	invisible(object)
}
