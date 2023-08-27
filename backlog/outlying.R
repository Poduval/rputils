
#' Find the observations with the largest Cook's distance
#' 
#' Utility function for the identification of the most influential observations.
#' Useful in the context of \code{\link{diagPlot}()}.
#' 
#' @param model \code{\link{lm}} or \code{IAZImodel}
#' @param n Integer: Number of observations to list.
#' @param what Character string specifying the columns to return: Choose between \code{"diag"} 
#' (the observation number and regression diagnostic measures obtained with \code{\link[IAZIgraph]{fortify.lm}}),
#' \code{"data"} (the observation number and the orginal data) or
#' \code{"both"} (observations, data and diagnostics).
#' 
#' @return
#' A data frame of the \code{n} most influential observations and columns according to parameter \code{what}.
#' 
#' @examples
#' mod <- lm(Sepal.Width ~ ., iris)
#' outlying(mod)
#' outlying(mod, n = 3, what = "both")
#' 
#' @importFrom utils head
#' @export
outlying <- function(model, n = 10, what = c("diag", "data", "both")){
  if (inherits(model, "lmIAZI")) model <- model$fit
  stopifnot(inherits(model, "lm"))
  what <- match.arg(what)
  infl <- fortify(model)
  if (hasWeights <- "(weights)" %in% names(infl)){
    infl <- dplyr::rename(infl, .weights = `(weights)`)
  }
  ind <- head(order(infl$.cooksd, decreasing = TRUE), n)
  diagCols <- c(if (hasWeights) ".weights", ".hat", ".sigma", ".cooksd", ".fitted", ".resid", ".stdresid")
  if (what == "diag"){
    out <- data.frame(ind = ind, infl[ind, diagCols])
  } else if (what == "data"){
    out <- data.frame(ind = ind, infl[ind, -match(diagCols, names(infl))])
  } else {
    out <- data.frame(ind = ind, infl[ind, ])
  }
  rownames(out) <- NULL
  out
}

