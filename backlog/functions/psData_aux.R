
#' Functions related to sensitivity plots (\code{psData})
#' 
#' @description
#' \code{psPlot(...)} is a convenience function combining \code{psData} and \code{plot}. 
#' It first creates a \code{psData} object and then applies the method \code{\link{plot.psData}} to it.
#' 
#' \code{NAline} adds a horizontal line to a sensitivity plot. 
#' \code{getColorCode} adds points of colors corresponding to the color code of the \code{varying} variable.
#' 
#' 
#' @param \dots In \code{psPlot}: passed to \code{\link{plot.psData}};
#' in \code{NAline()}: passed to \code{\link{geom_hline}}.
#' 
#' 
#' @details
#' \code{psPlot()} executes \code{plot(psData(}\emph{arguments}\code{), ...)}.
#' Note that the \code{\dots} go to \code{plot} - 
#' use \code{psDataArgs} to pass additional arguments to \code{psData()}.
#' 
#' \code{NAline()} adds a horizontal line at the level of the appraisal if the \code{varying} variable is \code{NA}
#' to a price sensitivity plot. 
#' Setting \code{NAline=TRUE} in a call to \code{\link{plot.psData}} will automatically trigger an appropriate call 
#' to \code{NAline()} and is usually more convenient than using the function \code{NAline} explicitly.
#' 
#' \code{getColorCodes()} adds points showing the color codes of the \code{varying} variable to a price sensitivity plot.
#' The colors green, orange and red are currently hard-coded and can not be changed.
#' Setting \code{colorCodes=TRUE} in a call to \code{\link{plot.psData}} is usually more convenient 
#' than using the function \code{getColorCodes()} explicitly.
#'  
#'
#' @return
#' \code{NAline()} and \code{getColorCode()} return graphical objects to be added to a ggplot.
#' 
#' @seealso \code{\link{psData}}  (also for examples!), \code{\link{psData_methods}}
#' 
#' @name psData_aux
NULL


#' @param model,muster,varying,fix,version Passed to \code{\link{psData}}.
#' @param psDataArgs Named list of additional arguments to be passed to \code{psData()} 
#' (e.g. \code{w}, \code{smooth}, \code{prodDate}, etc.).
#' 
#' @rdname psData_aux
#' @importFrom utils modifyList
#' @export
psPlot <- function(model, muster, varying, fix = NULL, version = NULL, 
                   psDataArgs = list(), ...){
  psDataCall <- as.call(modifyList(
    list(quote(psData), model = model, muster = muster, varying = varying, fix = fix, version = version), 
    psDataArgs))
  plot(eval.parent(psDataCall), ...)
}


#' @param x A \dQuote{psData}-object
#' @param colorList color(s) of lines
#' @param lty Passed to \code{\link{geom_hline}}.
#' 
#' @rdname psData_aux
#' @importFrom stats setNames update
#' @export
NAline <- function(x, colorList = c(IAZIdarkgreen, IAZIdarkblue, IAZIdarkred), lty = 2, ...){
  stopifnot(inherits(x, "psData"))
  grouped <- length(x$appraisal) > 1
  xupdated <- update(x, varying = setNames(list(NA), x$varyVar))
  if (grouped){
    data = data.frame(names(x$appraisal),
                      value = sapply(xupdated$appraisal, "[[", "value"))
    names(data)[[1]] <- xupdated$groupVar
    geom_hline(
      data = data,
      mapping = aes_(yintercept = quote(value), color = as.name(xupdated$groupVar)), 
      lty = lty, ...)
  } else {
    geom_hline(yintercept = xupdated$appraisal[[1]]$value, color = colorList[[1]], lty = lty, ...)
  }
}

 
#' @param x  A \code{psData} object
#' @param NAlines Level of possible NA lines in the plot. Used internally in \code{plot.psData()} 
#' to determine an appropriate vertical position of the color code symbols. 
#' @param d  Determines the vertical distance of the color symbols from the price sensitivity lines
#' (relative position on y axis range from bottom). Used internally in \code{plot.psData()}.
#' @param textCorrection Numeric constant. Corrects the position of the text labels horizontally. 
#' 
#' @rdname psData_aux
#' @importFrom utils tail
#' @export
getColorCodes <- function(x, NAlines = NULL, d = 0.05, textCorrection = 0){
  n.models <- length(x$appraisal)
  apprVal <- sapply(x$appraisal, "[[", "value")
  vvRange <- range(c(apprVal, NAlines), na.rm = TRUE)
  colorYPos <- rev(sapply(seq_len(n.models), function(i) vvRange[1] - d*i*diff(vvRange)))
  colorCodes <- lapply(x$appraisal, "[[", "varyVarColorCode")
  if (any(sapply(colorCodes, is.null))){
    noColors <- sapply(colorCodes, is.null)
    warning("No color codes for ", x$varyVar, " (", paste0(names(x$appraisal)[noColors], collapse = ", "), ")")
    colorCodes[noColors] <- list(rep("NA", nrow(x$data)))
  }
  colData <- data.frame(
    vv = rep(x$data[[x$varyVar]], n.models),
    colorCode = factor(
      unlist(colorCodes, use.names = FALSE),
      levels = c(0:2, "NA")),
    ypos = rep(colorYPos, each = nrow(x$data)))
  names(colData)[[1]] <- x$varyVar
  xvals <- x$data[[x$varyVar]]
  xpos <- rep(tail(xvals, 1) + diff(tail(xvals, 2))/2 + textCorrection, n.models)
  textData <- data.frame(
    x = xpos, y = colorYPos, label = names(colorCodes))
  colorsForColorScale <- c("green", "orange", "red", "gray40")[
                           match(c(0:2, "NA"), colData$colorCode, 0)>0]
  list(
    geom_hline(data = textData, mapping = aes_string(yintercept = "y"), col = "gray"),
    geom_point(data = colData, mapping = aes_string(x$varyVar, "ypos", fill = "colorCode"),
               col = "transparent", pch = 22),
    scale_fill_manual(values = colorsForColorScale),
    geom_text(data = textData, 
              mapping = aes_string("x", "y", label = "label"), 
              color = "black", hjust = 0, vjust = "middle", size = 3, 
              inherit.aes = FALSE))
}
