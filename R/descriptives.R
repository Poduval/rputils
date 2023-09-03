# ============================================================================ #
# Descriptive functions
# Technically these are wrappers on base functions like mean, quantile, etc.
# ============================================================================ #


#' @rdname describe
#' @name describe
#' @title alternative to summary function for vectors and data.frame
#' @description summary with additional details
#' @param x vector or data.frame
#' @param ... additional parameters
#'
#' @export
#'
describe <- function(x, ...) UseMethod("describe")

#' @rdname describe
#' @name describe
#' @title describe
#' @description calculates basic statistics of a given input vector
#'
#' @param x an input vector
#' @param q quantiles needs to be calculated, must be between (0, 1)
#' @param round_to rounding off the results
#'
#' @return returns a data.frame with descriptive
#' @examples
#' \dontrun{
#' describe(iris$Sepal.Length)
#' describe(iris$Sepal.Length, seq(0, 1, 0.10))
#' }
#'
#' @importFrom stats quantile sd median
#' @export
#'
describe.default <- function(x, q = seq(0, 1, 0.25), round_to = 2) {

  # quick check: input must be numeric
  stopifnot(class(x) %in% c('integer', 'numeric'),
            all(q >= 0 & q <= 1))

  # defining the quantifies for the function
  nms <- paste0("p", round(q * 100, 0))
  qnt <- round(quantile(x, probs = q, na.rm = T, names = F), round_to)
  qnt <- rbind.data.frame(qnt)
  names(qnt) <- nms
  d <- cbind.data.frame(n = length(x), nas = sum(is.na(x)),
                        avg = round(mean(x, na.rm = T), round_to),
                        med = round(median(x, na.rm = T), round_to),
                        std = round(sd(x, na.rm = T), round_to),
                        qnt)
  class(d) <- c("describe", "data.frame"); return(d)
}

#' @rdname describe
#' @name describe.data.frame
#' @description calculates basic statistics of a given input vector
#'
#' @param x data.frame
#' @param ... additional parameters accepted by \code{describe.default}s
#'
#' @return returns a data.frame with descriptive
#'
#' @examples
#' \dontrun{
#' describe(iris, q = c(0, 1))
#' describe(mtcars)
#' }
#' @export
#'
#'
describe.data.frame <- function(x, ...) {

  stopifnot(is.data.frame(x), nrow(x) >= 1, ncol(x) >= 1)
  # output - applied to only numerical values ====
  num_vars <- colnames(x)[!sapply(x, class) %in% c('character', 'factor')]
  if(length(num_vars)) {
    d <- do.call(rbind, apply(x[num_vars], 2, describe.default, ...))
    d <- cbind(variable = num_vars, d)
    row.names(d) <- NULL
    class(d) <- c("describe", "data.frame"); return(d)
  } else stop('No numeric fields observed in the data \n')
}

