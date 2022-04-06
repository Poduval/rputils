
# ===== fit model separating the two groups (using xgboost) ====

#' Fit a discrimination model to a \code{gDiff} object.
#' 
#' A model with binary response  \code{x$g} and all in \code{x$data} as explanatory (fetaure) variables is fit.
#' Available methods are \code{\link{ranger}}, \code{\link{xgboost}}, \code{\link{lm}}, \code{\link{glm}}.
#' 
#' @details 
#' \code{fit} is a generic function, and \code{fit.gDiff} is currently the only method.
#' 
#' In all \code{method}s, \code{x$g} is the response and all variables in \code{x$data} are 
#' independent variables. 
#' For \code{method="xgboost"}, the data \code{x$data} is first expanded to a matrix 
#' using \code{\link{reformulate}} and \code{\link{model.matrix}}.
#' 
#' For each method, additional parameters can be passed to the fitting function through the \code{\dots} argument.
#' Specific default are used depending on the function. 
#' These defaults can be overridden through \code{\dots}, too. 
#' \itemize{
#' \item{\code{method = "ranger"}: \code{\link{ranger}} is run with 
#' \code{num.trees = if (fast) 200 else 500}, \code{importance = "impurity"} 
#' and defaults otherwise.}
#' \item{\code{method = "xgboost"}: 
#' \code{\link{xgb.train}} is applied to appropriately prepared data with the following parameter settings:
#' \itemize{
#' \item{Default depending on \code{fast}:
#' \code{eta = if (fast) 0.05 else 0.01}, \code{nrounds = if (fast) 250 else 1000}, 
#' \code{early_stopping_rounds = if (fast) 10 else 25}, 
#' }
#' \item{Defaults not depending on \code{fast}:
#' \code{gamma = 10}, \code{verbose = 0}, \code{print_every_n = 10}
#' }
#' \item{The parameters \code{params}, \code{data}, \code{watchlist}, \code{booster}, 
#' \code{objective}, \code{eval_metric} can not be altered.
#' }
#' \item{For all others parameter of \code{\link{xgb.train}}, 
#' the defaults from the \code{xgboost} package are used,
#' but different values can be specified by the user.
#' }}}
#' \item{\code{method = "lm"}: By default, \code{model=FALSE}.}
#' \item{\code{method = "glm"}: By default, \code{family = "binomial"} and \code{model=FALSE}.
#' }
#' }
#' 
#'  
#' @param object A \code{\link{gDiff}}.
#' @param method method
#' @param prop.train Proportion of the data attributed in the training sample. 
#' The rest is used as test sample.
#' @param na.action na.action
#' @param fast Logical. \code{fast=TRUE} changes the default parameterization, 
#' resulting in lower execution time (and possibly a worse fit).
# @param eta,max_depth,gamma,subsample,colsample_bytree These parameters are added to the list 
# passed as the argument \code{params} to \code{\link{xgb.train}}.
# @param nrounds,early_stopping_rounds,verbose These parameters are passed to \code{\link{xgb.train}}.
#' @param \dots Argments passed to methods.
#' 
#' @return \code{fit.gDiff} returns an object of class \code{paste0("fittedgDiff_", method)}, inheriting
#' from the more general class \dQuote{fittedgDiff}. 
#' Available methods for these classes are described in the help topic \code{\link{fittedgDiff_methods}}.
#'
#' @seealso Methods: \code{\link{fittedgDiff_methods}}. Examples: \code{\link{gDiff-package}}.
#'
#' @name fit
NULL

#' @rdname fit
#' @export
fit <- function(object, ...) UseMethod("fit")

#' @rdname fit
#' @import xgboost
#' @importFrom ranger ranger
#' @importFrom stats lm glm model.matrix reformulate na.pass na.exclude
#' @importFrom dplyr n_distinct
#' @export
fit.gDiff <- function(object, method = c("ranger", "xgboost", "glm", "lm"), prop.train = 0.8, 
											na.action = if (method == "xgboost") na.pass else na.exclude, 
											fast = TRUE, ...){
  method <- match.arg(method)
  if (!(method %in% c("ranger", "xgboost")) && "fast" %in% names(match.call()[-1]))
  	message("Parameter 'fast' is ignored in fit(), as method=", method, ".")
  tm <- system.time(
    out <- switch(EXPR = method,
			ranger = fit_gDiff_default(object, method = method, prop.train = prop.train, 
								 na.action = na.action, 
								 additionalDefaults = list(num.trees = if (fast) 200 else 500, 
  							 													 importance = "impurity"), ...),
			xgboost = fit_gDiff_xgboost(object, prop.train = prop.train, fast = fast, 
																	additionalDefaults = list(
																		eta = if (fast) 0.05 else 0.01, 
																		nrounds = if (fast) 250 else 1000, 
																		early_stopping_rounds = if (fast) 10 else 25, 
																		gamma = 10, verbose = 0, print_every_n = 100),
																	...),
			lm = fit_gDiff_default(object, method = method, prop.train = prop.train, na.action = na.action, 
								 additionalDefaults = list(model = FALSE), 
								 ...),
			glm = fit_gDiff_default(object, method = method, prop.train = prop.train, na.action = na.action, 
								 additionalDefaults = list(family = "binomial", model = FALSE), 
								 ...)
			#, fit_gDiff_default(object, method = method, prop.train = prop.train, ...)
			)
    )[["elapsed"]]
  out$timing <- tm
  out$call <- sys.calls()[[sys.nframe() - 1]]
  out$envir <- parent.frame()
  out
}

#' @importFrom utils modifyList
fit_gDiff_default <- function(object, method, prop.train, na.action, 
															additionalDefaults = list(), ...){
  spl <- splitData(object$data, y = object$g, prop.train = prop.train)
	xx <- spl$x.train
	xx["_group_"] <- as.integer(spl$y.train) - 1L
	ntrain0 <- nrow(xx)
	xx <- na.action(xx)
	if (nrow(xx) < ntrain0){
		nremoved <- ntrain0 - nrow(xx)
		message(nremoved, " of ", ntrain0, " rows in training set not used due to NA's.")
	}
	n <- nrow(spl$x.train) + nrow(spl$x.test)
	cl <- quote(f(`_group_` ~ ., xx))
	cl[[1]] <- as.symbol(method)
	additionalArgs <- modifyList(additionalDefaults, list(...))
	cl <- modifyCall(cl, additionalArgs)
	out <- list(fit = eval(cl), train.index = spl$train.index)
	out$pred <- data.frame(group  = object$g,
												 sample  = factor(ifelse(seq_len(n) %in% spl$train.index, "train", "test"),
												 								 levels = c("train", "test"))
												 )
	class(out) <- c(paste0("fittedgDiff_", method), "fittedgDiff")
	out$pred$prediction <- NA
	dataComplete <- na.action(object$data)
	out$pred$prediction[match(rownames(dataComplete), rownames(object$data))] <- 
		predict(out, dataComplete)
	out
}


# xgboost case:
#' @importFrom utils modifyList
fit_gDiff_xgboost <- function(object, prop.train, fast = FALSE, additionalDefaults, ...){
  m <- model.matrix_na.pass(object$data)
  labels <- as.integer(object$g) - 1L
  spl <- splitData(m, y = labels, prop.train = prop.train)
 	n <- nrow(spl$x.train) + nrow(spl$x.test)

  # Transform the two data sets into xgb.Matrix
  DMat.train <- xgb.DMatrix(data = spl$x.train, label = spl$y.train)
  DMat.test <- xgb.DMatrix(data = spl$x.test, label = spl$y.test)
  
  # Define the parameters for multinomial classification
  num_class <- n_distinct(labels)

  # Prepare call to xgb.train
  cl <- quote(xgb.train(params = list(),
  											data = DMat.train,
  											watchlist = list(val1 = DMat.train,val2 = DMat.test),
  											booster = "gbtree",
  											objective = "binary:logistic",
  											eval_metric = "logloss"))
	additionalArgs <- modifyList(additionalDefaults, list(...))
	cl <- modifyCall(cl, additionalArgs)
  # Train the XGBoost classifer (~ 15min)
  xgb <- eval(cl)
  # Output
  out <- list(fit = xgb, train.index = spl$train.index)
	out$pred <- data.frame(group  = object$g,
												 sample = factor(ifelse(seq_len(n) %in% spl$train.index, "train", "test"),
												 				 			   levels = c("train", "test"))
												 )
  class(out) <- c("fittedgDiff_xgboost", "fittedgDiff")
 	out$pred$prediction <- predict(out, m)
	out
}

# Aux functions
model.matrix_na.pass <- function(x, vars = names(x)){
  nas <- is.na(x)
  x_noNA <- x
  impvalues <- lapply(x, function(x) x[!is.na(x)][[1]])
  x_noNA[] <- Map(function(x, i){x[is.na(x)] <- i; x}, x_noNA, impvalues)
  m <- model.matrix(reformulate(names(x_noNA), intercept = FALSE), x_noNA)
  assign <- attr(m, "assign")
  m[nas[, assign]] <- NA
  m
}
splitData <- function(x, y = NULL, prop.train = 0.8){
  n <- nrow(x)
  train.index <- sample(n, round(prop.train * n))
  stopifnot(length(train.index) > 0, length(train.index) < n)
  x.train <- x[train.index, , drop = FALSE]
  x.test <- x[-train.index, , drop = FALSE]
  out <- list(x.train = x.train, x.test = x.test)
  if (!is.null(y)){
    stopifnot(length(y) == n)
    y.train <- y[train.index]
    y.test <- y[-train.index]
    out <- c(out, list(y.train = y.train, y.test = y.test))
  }
  out$train.index <- train.index
  out
}

