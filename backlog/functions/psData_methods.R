
# ==== Methods for class "psData" ====
# plot
# c
# update

#' Methods for class \dQuote{psData}
#' 
#' @description
#' These are methods for plotting, combining and updating \code{\link{psData}} objects.
#' 
#' @param \dots In \code{c.psData()}: An enumeration of \code{psData} objects - see \dQuote{Details} for the requirements;
#' in \code{update.psData()}: arguments to be added or modified; 
#' in \code{plot.psData()}: not used.  
#' @param groupVarName Name of grouping variable. Used as title the legend if there are multiple version or models.
#' 
#' 
#' @details 
#' \code{plot.psData()} draws a price sensitivity plot. 
#' The case \code{discrete=TRUE && colorCodes=TRUE} is currently not implemented.
#' 
#' \code{c.psData()} combines several \code{psData} objects. 
#' All arguments in \code{\dots} must be \code{psData} objects,
#' they must have identical \code{$varying} components and they must be all named in the call
#' (as in \code{c(mod1 = psDataObj1, mod2 = psDataObj2)}).
#' The argument names in the call (\code{"mod1"} and \code{"mod2"}in the example) 
#' are used as labels of the models in the legend.
#' 
#' \code{update.psData()} updates a \code{psData} object. 
#' 
#' @return 
#' \code{plot.psData} returns a \code{ggplot}.
#' \code{c.psData} returns a \emph{grouped} version of psData, where \code{$groupVar} specifies 
#' the name of a grouping variable. 
#' \code{update.psData} returns a \code{psData} object.
#' 
#' @seealso \code{\link{psData}} (also for examples!), \link{psData_aux}
#' 
#' @name psData_methods
NULL


#' @param x,object A \code{psData} object.
#' @param discrete Logical: If \code{TRUE}, the \code{varying} variable is converted to a factor and the 
#' default geom (see argument \code{geom}) changes to \code{geom_bar}.
#' @param add Logical: If \code{TRUE}, the plot is added to the present ggplot; 
#' If \code{FALSE}, a new plot is generated
#' @param colorList Color of points or lines.
#' @param size Size of lines or points
#' @param NAline Logical: Inlcude a NA line in the plot? See \code{\link{NAline}}.
#' @param colorCodes logical: Show the color codes? See \code{\link{getColorCodes}}.
#' @param textCorrection Numeric constant: corrects the position of the release codes right of the quality codes horizontally. 
#' @param geom Character string specifying the geom to use. Possible choices are \code{"geom_line"},
#' \code{"geom_point"} and \code{"geom_bar"} (the latter only makes sense if \code{discrete=TRUE})
#' 
#' @rdname psData_methods
#' @export
plot.psData <- function(x, discrete = FALSE, add = FALSE,
                        colorList = c(IAZIdarkgreen, IAZIdarkblue, IAZIdarkred), 
                        size = 1, NAline = FALSE, colorCodes = FALSE, textCorrection = 0, 
                        geom = if (discrete) "geom_bar" else "geom_line",
                        groupVarName = x$groupVarName,
                        ...){
  if (colorCodes && (discrete || geom == "geom_bar")){
    stop("The case discrete=TRUE && colorCodes=TRUE is not implemented, as it causes confusion of color scales.")
  }
  grouped <- length(x$appraisal) > 1
  geom <- match.arg(geom, c("geom_line", "geom_bar", "geom_point"))
  appr <- unlist(lapply(x$appraisal, "[[", "value"), 
                 use.names = FALSE, recursive = FALSE)
  n_releases <- length(x$appraisal)
  n <- nrow(x$data)
  # discrete case: x <- factor
  if (discrete){
    x$data[[x$varyVar]] <- factor(x$data[[x$varyVar]])
  }
  # cbind x and appraisal data
  plotData <- cbind(x$data[rep(seq_len(n), n_releases), x$varyVar, drop = FALSE], 
                    appraisal = appr,
                    `_.groupVar._` = rep(names(x$appraisal), each = n))
  names(plotData)[[3]] <- x$groupVarName
  ok <- is.finite(plotData$appraisal)
  out <- list()
  # mapping
  if (!grouped){
    mapping <- aes_(as.name(x$varyVar), quote(appraisal))
  } else if (geom == "geom_bar"){
    mapping <- aes_(call("factor", as.name(x$varyVar)), quote(appraisal),
                    fill = as.name(x$groupVar), 
                    color = as.name(x$groupVar))
  } else {
    mapping <- aes_(as.name(x$varyVar), quote(appraisal), 
                    color = as.name(x$groupVar))
  }
  # call to geom_*
  call_geom <- call("fn", data = plotData, mapping = mapping, 
                    size = size)
  call_geom[[1]] <- as.name(geom)
  if (geom == "geom_bar"){
    call_geom$stat <- "identity"
    call_geom$alpha <- 0.6 
    if (!grouped){
      call_geom$fill <- call_geom$color <- colorList[[1]]
    } else {
      call_geom$position <- "dodge"
    }
  } else if (!grouped){
    call_geom$color <- colorList[[1]]
  }
  out <- list(eval(call_geom))
  
  if (NAline){
    NAlineGeom <- NAline(x, colorList = rep(colorList, length.out = n_releases))
    out <- c(out, list(NAlineGeom))
    NAlinelevels <- NAlineGeom$data$value
  } else {
    NAlinelevels <- NULL
  }
  if (colorCodes){
    out <- c(out, getColorCodes(x, NAlines = NAlinelevels, textCorrection = textCorrection))
  }
  
  if (any(!ok)){
    out <- c(out, 
             list(geom_vline(aes_(xintercept = as.name(x$varyVar)), 
                             data = x$data[!ok, x$varyVar, drop = FALSE], 
                             color = I(IAZIlightred), alpha = 0.9))
    )
  }
  out <- c(out, list(scale_color_manual(values = colorList)))
  if (grouped)
    out <- c(out, list(guides(color = guide_legend(title = groupVarName))))
  if (grouped && geom == "geom_bar" && !colorCodes) 
    out <- c(out, list(scale_fill_manual(values = colorList)))
  if (add) return(out)
  ggplot(data = out[[1]]$data, out[[1]]$mapping) + out
}



#' @rdname psData_methods
#' @export
c.psData <- function(..., groupVarName = "model"){
  dots <- list(...)
  # 'dots' arguments must be named
  if (is.null(names(dots)) || !all(nzchar(names(dots)))){
    stop("The '...' in c.psData() must all be named!")
  }
  # Check that all elements in '...' are "psData" objects
  stopifnot(sapply(dots, inherits, "psData"))
  # Check that 'varyVar', 'data[[varyVar]]' and 'groupVarName' are identical in all elements in '...'
  vvnm <- sapply(dots, "[[", "varyVar", USE.NAMES = FALSE)
  gvnm <- sapply(dots, "[[", "groupVarName", USE.NAMES = FALSE)
  stopifnot(is.character(vvnm), duplicated(vvnm)[-1])
  vvnm <- vvnm[[1]]
  vv.all <- lapply(dots, function(x) x$data[[vvnm]])
  stopifnot(duplicated(vv.all)[-1])
  # 'appraisal' elements
  apprElements <- lapply(unname(dots), "[[", "appraisal")
  l <- length(dots)
  ll <- lengths(apprElements)
  # Assemble new psData-object
  out <- list()
  out$data <- dots[[1]]$data[dots[[1]]$varyVar]
  out$appraisal <- do.call(c, apprElements)
  out$varyVar <- dots[[1]]$varyVar
  out$groupVarName <- groupVarName
  out$call <- match.call()
  out$calls <- unlist(lapply(dots, "[[", "calls"), use.names = FALSE)
  out$envir <- unname(do.call(c, lapply(dots, "[[", "envir")))
  if (l > 1 && any(ll > 1)){
    names(out$appraisal) <- paste(rep(names(dots), lengths(apprElements)),
                                  names(out$appraisal), 
                                  sep = "-")
  } else if (l > 1){
    names(out$appraisal) <- names(dots)
  } else if (any(ll > 1)){
    names(out$appraisal) <- names(apprElements[[1]])
  }
  names(out$calls) <- names(out$envir) <- names(out$appraisal)
  class(out) <- "psData"
  out
}

# ---- update method for 'psData' objects ----
#' @importFrom stats update
#' @rdname psData_methods
#' @export
update.psData <- function(object, ...){
  res <- vector("list", length(object$calls))
  names(res) <- names(object$calls)
  for (i in seq_along(object$calls)){
    cl <- updateCall(object$calls[[i]], ...)
    res[[i]] <- eval(cl, object$envir[[i]])
  }
  do.call(c, res)
}
# Auxiliary function - updates a call
updateCall <- function(cl, ...){
  updated <- update(list(call = cl), ..., evaluate = FALSE)
  dots <- list(...)
  updated[names(dots)] <- dots
  updated
}

