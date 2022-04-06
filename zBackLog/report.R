
#' Create a report of the distribution of an y variable at different levels of an x
#' 
#' @description
#' \code{report} generates a mainly graphical report of the distribution of a (usually numeric) \code{y} variable
#' considering an \code{x} variable that can be either numeric or categorical.
#' The graphical display uses a grouping of the \code{y} variable into categories \code{"low"}, \code{"inSample"} and \code{"high"}.
#' 
#' Typically, \code{y} contains model \emph{residuals} and \code{report} is used to check the model by inspecting
#' the residual distribution over the range of some regressor in the \code{x} variable.
#' 
#' @param object A formula, or a \code{data.frame} if you are using the alternative form 
#' \code{report(data, formula, ...)}.
#' @param data A data frame. 
#' @param what An integer vector, a subset of \code{1:5}: The outputs to produce, see section \dQuote{Value} below.
#' @param fillcolors A vector of length 4: the colors for graphs 3 and 4.
#' @param linecolor Color of vertical lines showing the \code{y_breaks} in output 2.
#' @param dup5,ylim5 Logical and numeric vector of length 2:
#' If \code{dup5=TRUE} the distribution plot in output 5 will be plotted a second time (as a 6th output)
#' zooming on the range \code{ylim5}. 
#' @param data_quoted Technical parameter. Do not specify.
#' @param \dots Arguments passed to methods
#' @param x \dQuote{report} object to be printed
#' @param print.info Logical: whether to print the text information in output 1.
#' @inheritParams prepXY
#' 
#' @details 
#' The formula must be two sided (\code{y~x}). 
#' 
#' \code{y} is usually a numeric variable, alternatively it can be a factor with levels 
#' \code{"low"}, \code{"inSample"}, \code{"high"} and possibly \code{"NA"}. 
#' Preprocessing of \code{y} is done by the auxiliary function \code{\link{.prepY}()},
#' according to the argument \code{y_breaks}. 
#' 
#' \code{x} can be either numeric or a factor. 
#' Preprocessing of \code{x} is done by the auxiliary function \code{\link{.prepX}},
#' according to the arguments \code{group_x}, \code{n_groups} and \code{x_breaks}. 
#' 
#' There are methods of \code{report.formula} and \code{report.data.frame}.
#' You can put either \code{report(formula, data, ...)} or \code{report(data, formula, ...)}.
#' The second form is useful for pipes.
#' 
#' \code{NA}s are allowed in \code{x} and \code{y}. 
#' In both cases they will be represented in a separate category (factor level).
#' 
#' @return 
#' \code{report} creates an object of class \dQuote{report}, a list with a selection of 5 outputs:
#' \enumerate{
#'   \item Frequency table of \code{x} groups (text/tabular output)
#'   \item Histogram of y (with group limits) - not produced by default.
#'   \item Bar chart displaying the \emph{absolute} frequencies of \code{y}-groups within \code{x}-groups.
#'   \item Bar chart displaying the \emph{relative} frequencies of \code{y}-groups within \code{x}-groups.
#'   \item Overlaid violin plots and boxplots of the distribution of \code{y} within \code{x}-groups.
#' }
#' By default output 1, 3, 4 and 5 are produced, but this selection can be altered with the argument \code{what}.
#' 
#' Note the strange fact that the output list will \emph{always have length 5} (or 6 if \code{dup5=TRUE}), 
#' even if only a subset of the outputs is required, but the list elements of not required outputs will be \code{NULL}.
#' 
#' @seealso \code{\link{prepXY}}
#' 
#' @examples
#' op <- par(ask = TRUE)
#' 
#' xvar <- sample(1:10, 100, replace = TRUE)
#' yvar <- rnorm(100)
#' 
#' # Produce all outputs and print single outputs:
#' myrep <- report(yvar~xvar, what = 1:5)
#' myrep[[1]]
#' myrep[[3]]
#' myrep[[5]]
#' 
#' # Case x a factor:
#' report(yvar~sample(LETTERS[1:3], 100, replace = TRUE), what = c(1, 3))
#' # custom y breaks:
#' report(yvar~xvar, y_breaks = c(-1, 1), what = c(1, 3))
#' # empty y categories:
#' report(yvar~xvar, y_breaks = c(-10, 10), what = c(1, 3))
#' # custom xvar breaks:
#' report(yvar~xvar, x_breaks = c(2, 8), what = c(1, 3))
#' # custom colors:
#' report(yvar~xvar, fillcolors = 1:3, what = 3)
#' # custom y axis in output 5:
#' report(yvar~xvar, what = 5, dup5 = TRUE, ylim5 = c(-1, 1))  # creates 2 plots
#' # factor y:
#' yfact <- factor(cut(yvar, c(-Inf, -1, 1, Inf), labels = c("low", "inSample", "high")))
#' report(yfact ~ xvar, what = 1:2)
#' 
#' 
#' # Model residuals:
#' mymodel <- lm(Sepal.Width ~ Petal.Length, iris)
#' resi <- resid(mymodel)
#' report(resi~Petal.Length, iris, n_groups = 6)
#' 
#' # same, in a pipe (uses the method report.data.frame)
#' library(dplyr)
#' iris %>% report(resi~Petal.Length, n_groups = 6)
#' 
#' # Specify transformations on lhs and rhs of formula
#' report(Petal.Length/Petal.Width ~ Sepal.Length/Sepal.Width, iris, n_groups = 6)
#' 
#' par(op)
#' 
#' @importFrom scales percent_format
#' @importFrom stats model.frame IQR median
#' @name report
NULL

#' @export
#' @rdname report
report <- function(object, ...){
  
  UseMethod("report")
}

#' Default method. Expects first formula then data
#' @rdname report 
#' @export
report.formula <- function(object, data, what = c(1, 3:5),
                           y_breaks = NULL, 
                           group_x = is.numeric(x), n_groups = 10, x_breaks = NULL, 
                           fillcolors = c(IAZIbrown, IAZIgreen, IAZIred, "darkgray"),
                           linecolor = IAZIred, dup5 = FALSE, ylim5 = NULL, 
                           data_quoted = NULL, ...
                           ){

  if (missing(object)) stop("argument 'formula' is missing")
  if (dup5 & is.null(ylim5)) stop("argument 'ylim5' is missing")
  if (!is.null(ylim5) && (ylim5[1] >= ylim5[2])) stop("Non sensical ylim5 values.")
  data_quoted <- match.call()$data
  formula <- object
  stopifnot(length(formula) == 3)
  x_quoted <- formula[[3]]
  y_quoted <- formula[[2]]
  
  if (missing(data)){
    data <- model.frame(formula)
  } else {
    data <- model.frame(formula, data)  
  }
  y <- data[[1]]
  x <- data[[2]]
  data$x_group <- .prepX(x, group_x = group_x, 
                         n_groups = n_groups, x_breaks = x_breaks)
  data$y_group <- .prepY(y, y_breaks = y_breaks)
  y_breaks <- attr(data$y_group, "y_breaks")
  
  stopifnot(length(fillcolors) == 4 || 
              (all(data$y_group!="NA") && length(fillcolors) == 3))
  fillcolors <- fillcolors[sort(unique(as.integer(data$y_group)))]
  
  out <- vector("list", 5)
  what <- intersect(what, 1:5)
  n.ymiss <- sum(is.na(y))

  if (1 %in% what){
    tbl <- as.data.frame(table(x_group = data[["x_group"]], exclude = NULL), responseName = "count")
    names(tbl) <- gsub("^x_group$", deparse(x_quoted)[1], names(tbl))
    out[[1]] <- tbl
  }

  if (2 %in% what){
    if (is.null(y_breaks)){
      plot1 <- ggplot(data, aes_string("y_group", color = "y_group", fill = "y_group")) + 
        geom_bar(alpha = 0.75) +  
        scale_fill_manual(values = fillcolors) + 
        scale_color_manual(values = fillcolors) +
        theme(legend.position = "none") + xlab("") +
        xlab(deparse(y_quoted)[1]) + 
        ggtitle_1("Frequency of y groups")
    } else {
      plot1 <- ggplot(data, aes_string("y")) + geom_histogram_1() +
        geom_vline(aes(xintercept = y_breaks[1]), col = linecolor, lwd = 1) +
        geom_vline(aes(xintercept = y_breaks[2]), col = linecolor, lwd = 1) + 
        xlab(deparse(y_quoted)[1]) + 
        ggtitle_1(paste("Distribution of", deparse(y_quoted)[1],
                        if (n.ymiss) paste0("(", n.ymiss, " obs. with missing y removed)")))
    }
    out[[2]] <- plot1
  }

  if (3 %in% what){
    plot2 <- ggplot(data, aes_string(x = "x_group", fill = "y_group")) +
      geom_bar_n(colors = fillcolors, position = position_stack(reverse = TRUE)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            legend.position = "bottom") +
      xlab(deparse(x_quoted)[1]) + 
      guides(fill = guide_legend(title = paste0(deparse(y_quoted)[1], ": ")), 
             reverse = TRUE) + 
      ggtitle_1(paste("Absolute frequencies in groups of", deparse(x_quoted)[1]))
    out[[3]] <- plot2
  }
  
  if (4 %in% what){
    plot3 <- ggplot(data, aes_string(x = "x_group", fill = "y_group")) +
      geom_bar_n(colors = fillcolors, position = position_fill(reverse = TRUE)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            legend.position = "bottom") +
      xlab(deparse(x_quoted)[1]) + 
      guides(fill = guide_legend(title = paste0(deparse(y_quoted)[1], ": "))) + 
      scale_y_continuous(labels = scales::percent_format()) + 
      ggtitle_1(paste("Relative frequencies in groups of", deparse(x_quoted)[1]))
    out[[4]] <- plot3 
  }

  if (5 %in% what && !is.null(y_breaks)){
    plot4 <- ggplot(data, aes_string("x_group", "y")) +
      geom_boxplot_1() +
      geom_violin_1(draw_quantiles = NULL) +
      geom_inSample_limits(y, data[["y_group"]], yintercept = median(y, na.rm = TRUE)) +
      facet_wrap(~x_group, scales = "free_x", nrow = 1) +
      scale_x_discrete(labels = NULL) +
      xlab(deparse(x_quoted)[1]) + ylab(deparse(y_quoted)[1]) +  
      theme(strip.text.x = element_text(angle = 90),
            axis.ticks = element_line(color = "transparent"),
            legend.position = "bottom") +
      ggtitle_1(paste("Distribution of", deparse(y_quoted)[1], "within groups of",
                      deparse(x_quoted)[1],
                      if (n.ymiss) paste0("(", n.ymiss, " obs. with missing y removed)")))
    if(!is.null(ylim5) && !dup5) plot4 <- plot4 + coord_cartesian(ylim=ylim5)
    out[[5]] <- plot4
  }
  
  if(dup5 && class(ylim5) == "numeric" && length(ylim5) == 2 && 5 %in% what && !is.null(y_breaks)) {
       out[[6]] <- plot4 + coord_cartesian(ylim=ylim5) +  
                           ggtitle_1(paste("Distribution of", deparse(y_quoted)[1], "within groups of",
                                     deparse(x_quoted)[1],
                                     if (n.ymiss) paste0("(", n.ymiss, " obs. with missing y removed)"), "\nZoomed In"))
    }
    
  structure(out, 
            x = x_quoted, y = y_quoted, data_quoted = data_quoted,
            y_breaks = y_breaks,
            y_freqs = table(data$y_group, exclude = NULL),
            class = c("list", "report"))
}


# 'formula' method. Expects first formula then \code{data.frame}
# @rdname report
# @export
# report.formula <- function(object, data, ...){
#   data_quoted <- match.call()$data
#   report.default(object = object, data = data, ..., data_quoted = data_quoted)
# }

#' 'data' method. Expects first \code{data.frame} then formula
#' @rdname report
#' @export
report.data.frame <- function(object, ...){
  data_quoted <- match.call()$object
  formula <- match.call(report.formula)$data
  data <- object
  vars <- all.vars(formula)
  if (!all(vars %in% names(data))){
    addvars <- setdiff(vars, names(data))
    env <- environment(formula)
    if (is.null(env)) env <- globalenv()
    data[addvars] <- mget(addvars, envir = env, inherits = TRUE)
  }
  report.formula(formula = formula, data = data, ..., data_quoted = data_quoted)
}

#' print.report method
#' @rdname report
#' @export
print.report <- function(x, print.info = inherits(x[[1]], "data.frame"), ...){
  if (print.info){
    attr_x <- attributes(x)
    if (is.null(attr_x$data_quoted)) attr_x$data_quoted <- "(none)"
    writeLines("=== Bivariate Summary Report ===\n--------------------------------")
    writeLines(paste0(
      format(c("data set:", "x variable:", "y variable:")),
      "   ", 
      as.character(c(attr_x$data_quoted, attr_x$x, attr_x$y))))
    cat("\n")
    ylvls <- names(attr_x$y_freqs)
    breaksInfo <- rep("", length(attr_x$ylvls))
    if (!is.null(attr(x, "y_breaks"))){
      breaksInfo[1:3] <- 
        c(paste0(" (y<=", attr_x$y_breaks[1], ")"),
          paste0(" (", attr_x$y_breaks[1], "<y<=", attr_x$y_breaks[2], ")"),
          paste0(" (y>", attr_x$y_breaks[2], ")"))
    }
    writeLines(paste0(
      format(rep(c("y categories:", ""), c(1, (length(ylvls)-1)))),
      format(paste0(" ", format(ylvls), 
                    breaksInfo)),
      ": ",
      format(attr_x$y_freqs[ylvls]),
      " (",
      format(attr_x$y_freqs[ylvls] / sum(attr_x$y_freqs) * 100,
             digits = 2),
      "%)"
    ))
  }
  for (xi in x){
    if (is.data.frame(xi)){
      cat("\nx-groups (with frequencies):\n")
      print(xi, row.names = FALSE, right = FALSE, ...)
    } else if (!is.null(xi)){
      print(xi, ...)
    }
  }
  invisible()
}
`[.report` <- function(x, ...){
  cl <- match.call()
  cl[[1]] <- `[`
  cl$x <- quote(unclass(x))
  out <- eval(cl)
  attributes(out) <- attributes(x)
  out
}

