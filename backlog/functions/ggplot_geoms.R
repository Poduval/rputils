
# ==== ggplot templates ====

#' Add a title to a \code{ggplot} graphic
#' 
#' Unlike \code{\link{ggtitle}}, \code{ggtitle_1} adds one or several blank lines 
#' before the title. This provides a workaround to creating vertical space between graphics
#' in Rmarkdown reports.
#' @param label,\dots passed to \code{\link{ggtitle}}.
#' @param spaces Postive integer: Number of leading blank lines to include in the title.
#' 
#' @import ggplot2
#' @export
ggtitle_1 <- function(label, ..., spaces = 1){
  label <- paste0(c(rep("\n", spaces), label), collapse = "")
  ggtitle(label, ...)
}


# Customized geom functions
# -------------------------

#' ggplot \code{geom}'s with modified defaults
#' 
#' @param \dots,fill,fill_color,alpha,color Passed to \code{geom_}\emph{xy} functions
#' from the \code{ggplot2} package.
#' @param draw_quantiles Passed to \code{\link{geom_violin}}.
#' @param varwidth Passed to \code{\link{geom_boxplot}}.
#' @param colors List of colors for color scale.
#' 
#' @details 
#' \code{geom_histogram_1} is \code{\link{geom_histogram}} with modified default.
#' \code{geom_bar_1} is \code{\link{geom_bar}} with modified default.
#' \code{geom_violin_1} is \code{\link{geom_violin}} with modified default.
#' \code{geom_boxplot_1} is \code{\link{geom_boxplot}} with modified default.
#' 
#' \code{geom_bar_n} is similar to \code{geom_bar_1}, but with an additional
#' categorigal variable (mapped as \code{fill} in \code{aes()}) rendered using the color scale specified in \code{colors}.
#' Accordingly, the number of colors in \code{colors} must be equal to or larger than
#' the number of levels of the \code{fill} variable. 
#' 
#' @examples 
#' # One numeric variable
#' ggplot(iris, aes(Sepal.Width)) + geom_histogram_1()
#' 
#' # One categorial variable
#' ggplot(iris, aes(Species)) + geom_bar_1()
#' 
#' # Two numeric variables -> see ?geom_hex_1
#' 
#' # Two variables, one numeric, one categorial:
#' ggplot(iris, aes(Species, Sepal.Width)) + geom_violin_1()
#' ggplot(iris, aes(Species, Sepal.Width)) + geom_boxplot_1()
#' # Very informative plots (often used in reports) are obtained by 'adding'
#' # geom_violin_1 and geom_boxplot_1:
#' ggplot(iris, aes(Species, Sepal.Width)) + geom_violin_1() + geom_boxplot_1()
#' 
#' # Alternative way to display one numeric and one categorial variable:
#' ggplot(iris, aes(Sepal.Width, fill = Species)) + 
#'   geom_bar_n(colors = c(IAZIdarkgreen, IAZIdarkblue, IAZIblue))
#'   
#' @seealso \code{\link{geom_hex_1}}.
#' 
#' @name geom_functions
NULL

# histogram_1 [x (continuous)]
#' @rdname geom_functions
#' @export
geom_histogram_1 <- function(..., color = IAZIdarkblue, fill = IAZIblue, alpha = 0.4){
  geom_histogram(..., color = color, fill = fill, alpha = alpha)
}

# bar_1 [x (discrete)]
#' @rdname geom_functions
#' @export
geom_bar_1 <- function(..., fill_color = IAZIdarkblue, alpha = 0.4)
  geom_bar(..., fill = fill_color, alpha = alpha)


# violin_1 [aes mapped: x (categorical), y (continuous)]
#' @rdname geom_functions
#' @export
geom_violin_1 <- function(..., color = IAZIdarkgreen, fill = IAZIdarkgreen, 
                          alpha = 0.2, draw_quantiles = 0.5){
  geom_violin(..., color = color, fill = fill, alpha = alpha, draw_quantiles = draw_quantiles)
}


# boxplot_1 [aes mapped: x (categorical), y (continuous)]
#' @rdname geom_functions
#' @export
geom_boxplot_1 <- function(..., varwidth = TRUE, color = IAZIblue, fill = IAZIlightblue, alpha = 0.2){
  geom_boxplot(..., varwidth = varwidth, color = color, fill = fill, alpha = alpha)
}

# bar_n [x (discrete), fill (categorical)]
# length(colors) should at least = number of categories in 'fill' variable
#' @rdname geom_functions
#' @export
geom_bar_n <- function(..., colors = c(IAZIdarkgreen, IAZIdarkblue), alpha = 0.6){
 list(geom_bar(..., alpha = alpha),
      scale_fill_manual(values = colors))
}


# hex_1 [x, y: continuous]

#' Hexbin plot, with an optional smoothing line
#' 
#' @param \dots,bins,binwidth passed to \code{\link{geom_hex}}.
#' @param smooth Logical: If \code{TRUE}, a smoothing line will be added to the plot 
#' (with \code{\link{geom_smooth}})
#' @param smooth_method Passed to \code{\link{geom_smooth}} if \code{smooth=TRUE}.
#' @param col_line Color of the smoothing line.
#' @param fill_colors Colors for the color scale indicating point density.
#' 
#' @examples
#' n <- 1000
#' x <- rnorm(n, sd=2)
#' y <- sin(x) + rnorm(n)
#' d <- data.frame(x, y)
#' ggplot(d, aes(x, y)) + geom_hex_1()
#' ggplot(d, aes(x, y)) + geom_hex_1(smooth = FALSE)        # no smoothing
#' ggplot(d, aes(x, y)) + geom_hex_1(smooth_method = "gam") # different smoothing method
#' 
#' @export
geom_hex_1 <- function(..., bins = 30, binwidth = NULL, smooth = TRUE, smooth_method = "auto",
                       fill_colors = c(IAZIlightgreen, IAZIgreen, IAZIblue, IAZIdarkblue),
                       col_line = IAZIred){
  list(
    geom_hex(..., bins = bins, binwidth = binwidth), 
    if (smooth) geom_smooth(method = smooth_method, color = col_line), 
    scale_fill_gradientn(colors = fill_colors, trans = "log10")
  )
}


#' Add horizontal reference lines to a plot
#' 
#' \code{geom_inSample_limits} adds three horizontal lines to an existing plot: 
#' one at 0 (or any other prespecified value) and 
#' two at the limits of the groups \code{"low"}, \code{"inSample"} and \code{"high"} of 
#' \code{inSample}.
#' 
#' @param resi A numeric vector, typically residuals.
#' @param inSample A factor or character vector of the same length as \code{resi},
#' with levels \code{"low"}, \code{"inSample"} and \code{"high"}.
#' @param col_lines Color of the reference lines
#' @param yintercept Level of the central reference line.
#' @param na.rm Used in calculation of the limits between the groups.
#' 
#' @details 
#' Note that although its name begin with \code{"geom_"}, variable mapping using \code{\link{aes}} 
#' is not implemented in this function; \code{resi} and \code{inSample} are specified in the traditional
#' R way, i.e. simply as vectors.
#' 
#' @examples
#' # Example with model residuals (the usual context of this function)
#' mylm <- lm(Sepal.Width ~ ., iris)
#' dataAndResid <- data.frame(iris, resid = resid(mylm))
#' inSampleGroups <- 
#'   factor(cut(dataAndResid$resi, c(-Inf, -.4, .4, Inf)),
#'          labels = c("low", "inSample", "high"))
#' table(inSampleGroups)
#' ggplot(dataAndResid, aes(Petal.Length, resid)) + geom_point(color = IAZIdarkblue, cex = 2) +
#'   geom_inSample_limits(dataAndResid$resid, inSampleGroups)
#' 
#' ggplot(dataAndResid, aes(Species, resid)) + geom_boxplot_1() + geom_violin_1() +
#'   geom_inSample_limits(dataAndResid$resid, inSampleGroups)
#'   
#' # A less typical example: yintercept specified explicitely
#' # Create three groups according to Petal.Width
#' groups <- cut(iris$Petal.Width, 0:3)
#' levels(groups) <- c("low", "inSample", "high")
#' # Plot including reference lines
#' ggplot(iris, aes(Sepal.Length, Petal.Width)) + geom_point() + 
#'   geom_inSample_limits(iris$Petal.Width, groups, yintercept = 1.5)
#' 
#' @export
geom_inSample_limits <- function(resi, inSample, col_lines = IAZIred, 
                                 yintercept = 0, na.rm = FALSE){
  if (!is.factor(inSample)) inSample <- factor(inSample)
  stopifnot(c("low", "inSample", "high") %in% levels(inSample))
  inSample_limits <- tapply(resi, inSample, range, na.rm = na.rm)
  inSample_lines <- c(
    low = mean(c(inSample_limits$low[2], inSample_limits$inSample[1]), na.rm = na.rm),
    high = mean(c(inSample_limits$inSample[2], inSample_limits$high[1]), na.rm = na.rm))
  list(geom_hline(aes_q(yintercept = yintercept), color = col_lines),
       geom_hline(data = data.frame(ref = inSample_lines), 
                  aes_string(yintercept = "ref"), color = col_lines, lty = 2))
}

