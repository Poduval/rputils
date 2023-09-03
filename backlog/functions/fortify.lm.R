
#' Amended version of \code{fortify.lm} (originally from ggplot2)
#' 
#' Supplements the data fitted to a linear model with model fit statistics
#' 
#' \code{ggplot2:::fortify.lm} breaks down in case of a weighted linear model if 
#' some cases have weights zero (unless the model has \code{na.action = na.exclude}).
#' Our version of the method fixes that. Note that zero-weight cases will have \code{NA} values
#' in some columns. 
#' 
#' @param model A \code{lm} object.
#' @param data data set, defaults to data used to fit model.
#' @param \dots not used.
#' 
#' @seealso \code{ggplot2::\link[ggplot2]{fortify.lm}}
#' 
#' @examples
#' fort <- fortify(lm(Sepal.Width ~ ., iris))
#' head(fort)
#' 
#' @export
fortify.lm <- function (model, data = model$model, ...){
  infl <- stats::influence(model, do.coef = FALSE)
  pos_weight <- if (is.null(model$weights)) TRUE else model$weights > 0
  data$.hat <- NA_real_
  data$.hat[pos_weight] <- infl$hat
  data$.sigma <- NA_real_
  data$.sigma[pos_weight] <- infl$sigma
  data$.cooksd <- NA_real_
  data$.cooksd[pos_weight] <- stats::cooks.distance(model, infl)
  data$.fitted <- stats::predict(model)
  data$.resid <- stats::resid(model)
  data$.stdresid <- NA_real_
  data$.stdresid[pos_weight] <- stats::rstandard(model, infl)
  data
}
