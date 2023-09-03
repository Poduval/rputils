
#' Update a fit, possibly with shuffled data
#' 
#' Re-run \code{\link{fit}}, possibly with different parameters.
#' 
#' \code{update.fittedgDiff} and \code{shuffle} are essentially identical,
#' differing in the default of the parameter \code{shuffle} only.
#' 
#' @param object,x object of class \dQuote{fittedgDiff}
#' @param shuffle Logical
#' @param \dots Currently not used.
#' 
#' @name update
NULL

#' @rdname update
#' @importFrom stats update update.default
#' @export
update.fittedgDiff <- function(object, shuffle = FALSE, ...){
	if (!shuffle) return(update.default(object, ...))
	cl <-  match.call(fit, object$call)
	cl <- modifyCall(cl, list(...))
	objName <- cl$object
	shuffled.gDiff <- eval(objName, object$envir)
	shuffled.gDiff$g <- sample(shuffled.gDiff$g)
	cl$object <- quote(shuffled.gDiff)
	eval(cl, list(shuffled.gDiff = shuffled.gDiff), 
			 enclos = object$envir)
}

#' @rdname update
#' @export
shuffle <- function(x, shuffle = TRUE, ...){
	stopifnot(inherits(x, "fittedgDiff"))
	update(x, shuffle = shuffle)
}

