
# Source of parts of the ggplot version of plot.lm: https://rpubs.com/therimalaya/43190
# Modified and extended by MAM

#' IAZI-style version of \code{\link{plot.lm}}
#' 
#' @param model A model inheriting either from \code{\link{lm}} or from \code{lmIAZI}.
#' @param which Integer vector, a subset of \code{1:9}: 
#' which diagnostic plots to draw. See Details.
#' @param hex Logical: If \code{TRUE}, some of the plots are displayed using hexbin instead of points.
#' @param smooth Logical: If \code{TRUE}, a smoothing line will be added to the plots 
#' with \code{\link{geom_smooth}()}.
#' @param smoothMethod Passed to \code{\link{geom_smooth}}.
#' @param pointCol,refLineCol,barFillCol,smoothLineCol,smoothFillCol Colors of graphical elements.
#' 
#' @details
#' The available plots are 
#' \enumerate{
#' \item{Residual vs Fitted Plot}
#' \item{Normal Q-Q}
#' \item{Scale-Location}
#' \item{Cook's distance}
#' \item{Residual vs Leverage Plot}
#' \item{Cook's dist vs Leverage hii/(1-hii)}
#' \item{Histogram of Weights}
#' \item{Weights vs Residuals Plot}
#' \item{Weights vs Leverage Plot}
#' }
#' Plots 7, 8 and 9 are only available for weighted models.
#'
#' Leverage, Cook's Distance, etc. are calculated using \code{\link[IAZIgraph]{fortify.lm}}.
#' 
#' If there are zero-weight cases in a weighted linear model, these points have \code{NA}s in some of
#' the diagnostics, and will not be displayed in some of the plots. 
#' The plots with some data points missing are marked with an asterisk in the plot title.
#' 
#'      
#' @return Returns a list of ggplot-objects with class \dQuote{gglist}.
#' See \code{?\link{gglist}} for options for customizing the layout of the output.
#' 
#' @seealso
#' \code{\link{outlying}}, \code{\link{gglist}}, \code{\link[IAZIgraph]{fortify.lm}} 
#' 
#' @examples
#' # lm with iris data
#' mod <- lm(Sepal.Width ~ ., iris)
#' 
#' # standard diagnostics plots
#' par(mfrow = c(2, 2))
#' plot(mod)
#' par(mfrow = 2:3)
#' plot(mod, 1:6)
#' # IAZI-style versions
#' x <- diagPlot(mod)
#' x  # same as plot(x)
#' str(x)
#' x[1:2]
#' print(x[1:2], nrow = 1) # see ?gglist
#' 
#' diagPlot(mod, smooth = FALSE)
#' diagPlot(mod, hex = TRUE)
#' diagPlot(mod, smooth = FALSE, hex = TRUE)
#' 
#' x <- diagPlot(mod, which = 1:6)
#' x
#' plot(x, nrow = 2)
#' 
#' # Data with an outlier
#' iris_with_outlier <- iris
#' iris_with_outlier[sample(150, 4), 1] <- c(3, 3, 6, 8) # 5 potential outliers
#' # weighted model, downweighting outliers:
#' w <- robustbase::lmrob(Sepal.Length ~ ., iris_with_outlier)$rweights
#' wmod <- lm(Sepal.Length ~ ., iris_with_outlier, weights = w)
#' diagPlot(wmod)
#' diagPlot(wmod, 1:9)
#' # Identify the most influential observations (according to Cook'D)
#' outlying(wmod, 5)
#' 
#' @importFrom utils globalVariables
#' @importFrom dplyr rename select
#' @importFrom stats qqnorm qqnorm 
#' @export
diagPlot <- function(model, which = c(1:3, 5, if (hasWeights) 7:8), 
                     hex = n>5000, 
                     smooth = TRUE, smoothMethod = "auto",
                     pointCol = IAZIblue, refLineCol = IAZIred, barFillCol = IAZIlightblue,
                     smoothLineCol = IAZIdarkgreen, smoothFillCol = "lightgray"){
  
  if (inherits(model, "lmIAZI")) model <- model$fit
  stopifnot(inherits(model, "lm"))
  n <- length(model$residuals)
  
  fmodel <- ggplot2::fortify(model)
  if (hasWeights <- !is.null(model$weights)){
    fmodel <- rename(fmodel, .weights = `(weights)`)
    fmodel <- transform(fmodel, posWt = factor(ifelse(.weights==0, "w=0", "w>0"), levels = c("w>0", "w=0")))
  }
  zero_weight <- if (hasWeights) model$weights == 0 else rep(FALSE, nrow(model$model))
  
  titles <- c("Residual vs Fitted Plot", "Normal Q-Q", "Scale-Location", "Cook's distance", 
              "Residual vs Leverage Plot", "Cook's dist vs Leverage hii/(1-hii)", 
              if (hasWeights) 
                c("Histogram of Weights", "Weights vs Residuals Plot", "Weights vs Leverage Plot"))
  title_append <- if (hasWeights) paste0("(*", sum(zero_weight), " obs with w=0 not shown)") else NULL
  if (hasWeights && any(c(2:6, 9) %in% which)){
    j <- intersect(c(2:6, 9), which)[[1]]
    titles[j] <- paste(titles[j], title_append)
    titles[setdiff(c(2:6, 9), j)] <- paste0(titles[setdiff(c(2:6, 9), j)], "*")
  }
  
  which <- intersect(sort(which), if (hasWeights) 1:9 else 1:6)
  out <- vector("list", length(which))
  nms <- c("rvfPlot", "qqPlot", "sclLocPlot", "cdPlot", "rvlevPlot", "cvlPlot", 
           if (hasWeights) c("wHist", "wvrPlot", "wvlevPlot"))
  names(out) <- nms[which]
  attr(out, "call") <- match.call()
  class(out) <- "gglist"
    
  if (1 %in% which){
    ggpl <- if (hasWeights){
      ggplot(dplyr::select(fmodel, -posWt), 
             aes(.fitted, .resid, color = .weights)) + 
        if (!hex) scale_colour_gradientn(colors = c(IAZIgreen, IAZIblue, IAZIdarkblue))
      } else {
        ggplot(fmodel, aes(.fitted, .resid))
      }
    if (hex){
      ggpl <- ggpl + geom_hex_1(smooth = FALSE)
    } else {  
      ggpl <- ggpl + 
        if (hasWeights) list(geom_point(data = fmodel, 
                                        mapping = aes(shape = posWt), na.rm = TRUE), 
                             scale_shape_manual(values = c(16, 4)))
        else geom_point(color = pointCol, na.rm = TRUE)
    }
    if (smooth)
      ggpl <- ggpl + geom_smooth(method = smoothMethod, color = smoothLineCol, fill = smoothFillCol, na.rm = TRUE) 
    ggpl <- ggpl + geom_hline(yintercept = 0, col = refLineCol, linetype = "dashed")
    ggpl <- ggpl + xlab("Fitted values") + ylab("Residuals")
    ggpl <- ggpl + ggtitle(titles[1]) + theme_bw() +
      guides(shape = guide_legend(title = element_blank()))
    out[[nms[1]]] <- ggpl
  }
  
  if (2 %in% which){
    ggpl <- if (hasWeights){
      ggplot(fmodel[!zero_weight, ], 
             aes(qqnorm(.stdresid, plot.it = FALSE)[[1]], .stdresid, color = .weights)) +
        geom_point(na.rm = TRUE) +
        scale_colour_gradientn(colors = c(IAZIgreen, IAZIblue, IAZIdarkblue))
    } else { 
      ggplot(fmodel, aes(qqnorm(.stdresid, plot.it = FALSE)[[1]], .stdresid)) +
        geom_point(color = pointCol, na.rm = TRUE)
    }
    ggpl <- ggpl + geom_abline(data = qqline1(fmodel$.stdresid), 
                               aes(intercept = intercept, slope = slope),
                               col = refLineCol, linetype = "dashed")  + 
            xlab("Theoretical Quantiles") + ylab("Standardized Residuals")
    ggpl <- ggpl + ggtitle(titles[2]) + theme_bw()
    out[[nms[2]]] <- ggpl
  }
  
  if (3 %in% which){
    ggpl <- if (hasWeights){
      ggplot(fmodel[!zero_weight, ], 
             aes(.fitted, sqrt(abs(.stdresid)), color = .weights)) + 
        if (!hex) scale_colour_gradientn(colors = c(IAZIgreen, IAZIblue, IAZIdarkblue))
    } else {
      ggplot(fmodel, aes(.fitted, sqrt(abs(.stdresid))))
    }
    if (hex){
      ggpl <- ggpl + geom_hex_1(smooth = FALSE)
    } else {  
      ggpl <- ggpl + if (hasWeights) geom_point(na.rm = TRUE) else geom_point(color = pointCol, na.rm = TRUE)
    }
    if (smooth)
      ggpl <- ggpl + geom_smooth(method = smoothMethod, color = smoothLineCol, fill = smoothFillCol, na.rm = TRUE) 
    ggpl <- ggpl + xlab("Fitted Value") + ylab(expression(sqrt("|Standardized residuals|")))
    ggpl <- ggpl + ggtitle(titles[3]) + theme_bw()
    out[[nms[3]]] <- ggpl
  }
  
  if (4 %in% which){
    ggpl <- if (hasWeights){
      ggplot(fmodel[!zero_weight, ], 
             aes(seq_along(.cooksd), .cooksd, fill = .weights)) + 
        scale_fill_gradientn(colors = c(IAZIgreen, IAZIblue, IAZIdarkblue)) +
        geom_bar(stat = "identity", position = "identity")
    } else {
      ggplot(fmodel, aes(seq_along(.cooksd), .cooksd)) + 
        geom_bar(stat = "identity", position = "identity", fill = barFillCol)
    }
    ggpl <- ggpl + xlab("Obs. Number") + ylab("Cook's distance")
    ggpl <- ggpl + ggtitle(titles[4]) + theme_bw()
    out[[nms[4]]] <- ggpl
  }
  
  if (5 %in% which){
    ggpl <- ggplot(fmodel[!zero_weight, ], aes(.hat, .stdresid)) + 
          geom_point(aes(color = .cooksd), na.rm = TRUE) +
          scale_colour_gradientn(colors = c(IAZIgreen, IAZIblue, IAZIdarkblue))
    if (smooth) 
      ggpl <- ggpl + geom_smooth(method = smoothMethod, color = smoothLineCol, fill = smoothFillCol, na.rm = TRUE)
    ggpl <- ggpl + geom_hline(yintercept = 0, col = refLineCol, linetype = "dashed")
    ggpl <- ggpl + xlab("Leverage") + ylab("Standardized Residuals")
    ggpl <- ggpl + ggtitle(titles[5])
    ggpl <- ggpl + scale_size_continuous("Cook's Distance", range = c(1,5))
    ggpl <- ggpl + theme_bw() + theme(legend.position = "right")
    out[[nms[5]]] <- ggpl
  }
  
  if (6 %in% which){
    ggpl <- if (hasWeights){
        ggplot(fmodel[!zero_weight, ], aes(.hat, .cooksd, color = .weights)) + 
          geom_point(na.rm = TRUE) +
          scale_colour_gradientn(colors = c(IAZIgreen, IAZIblue, IAZIdarkblue))
      } else {
        ggplot(fmodel, aes(.hat, .cooksd)) + 
          geom_point(color = pointCol, na.rm = TRUE)
      }
    if (smooth)
      ggpl <- ggpl + geom_smooth(method = smoothMethod, color = smoothLineCol, fill = smoothFillCol, na.rm = TRUE)
    ggpl <- ggpl + xlab("Leverage hii") + ylab("Cook's Distance")
    ggpl <- ggpl + ggtitle(titles[6])
    ggpl <- ggpl + geom_abline(slope = seq(0,3,0.5), color = refLineCol, linetype = "dashed")
    ggpl <- ggpl + theme_bw()
    out[[nms[6]]] <- ggpl
  }

  if (7 %in% which){
    ggpl <- ggplot(fmodel, aes(.weights)) + 
      geom_histogram(col = pointCol, fill = barFillCol, breaks = seq(0, 1, if (n < 150) .05 else 0.025), 
                     na.rm = TRUE)
    ggpl <- ggpl + geom_vline(xintercept = 0:1, col = refLineCol, linetype = "dashed")
    ggpl <- ggpl + xlab("Weights")
    ggpl <- ggpl + ggtitle(titles[7]) + theme_bw()
    out[[nms[7]]] <- ggpl
  }

  if (8 %in% which){
    ggpl <- ggplot(fmodel, aes(.resid, .weights))
    if (hex){
      ggpl <- ggpl + geom_hex_1(smooth = FALSE)
    } else {  
      ggpl <- ggpl + geom_point(mapping = aes(shape = posWt), 
                                color = pointCol, na.rm = TRUE) + 
        if (hasWeights) scale_shape_manual(values = c(16, 4))
    }
    ggpl <- ggpl + geom_hline(yintercept = 0:1, col = refLineCol, linetype = "dashed")
    ggpl <- ggpl + xlab("Residuals") + ylab("Weights")
    ggpl <- ggpl + ggtitle(titles[8]) + theme_bw()  +
      guides(shape = guide_legend(title = element_blank()))
    out[[nms[8]]] <- ggpl
  }

  if (9 %in% which){
    ggpl <- ggplot(fmodel[!zero_weight, ], aes(.hat, .weights))
    if (hex){
      ggpl <- ggpl + geom_hex_1(smooth = FALSE)
    } else {  
    ggpl <- ggpl + geom_point(color = pointCol, na.rm = TRUE)
    }
    ggpl <- ggpl + geom_hline(yintercept = 0:1, col = refLineCol, linetype = "dashed")
    ggpl <- ggpl + xlab("Leverage") + ylab("Weights")
    ggpl <- ggpl + ggtitle(titles[9]) + theme_bw()
    out[[nms[9]]] <- ggpl
  }
    
  return(out)
}
# Auxiliary function required for QQ-plot
#' @importFrom stats quantile qnorm
qqline1 <- function (y, datax = FALSE, distribution = qnorm, probs = c(0.25, 
    0.75), qtype = 7, ...){
  stopifnot(length(probs) == 2, is.function(distribution))
  y <- quantile(y, probs, names = FALSE, type = qtype, na.rm = TRUE)
  x <- distribution(probs)
  if (datax) {
    slope <- diff(x)/diff(y)
    int <- x[1L] - slope * y[1L]
  }
  else {
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope * x[1L]
  }
  data.frame(intercept = int, slope = slope, ...)
}


globalVariables(names = c("(weights)", ".cooksd", ".fitted", ".hat", ".resid", ".stdresid", ".weights", 
                          "intercept", "posWt", "slope"))
