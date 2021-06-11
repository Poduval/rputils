#' rp_desc
#' @description calculates basic statistics of a give inout vector
#'
#' @param x an input vector
#' @param pr quantiles needs to be calculated, must be between (0, 1)
#' @param round_to rounding off the results
#' @importFrom stats quantile sd
#'
#' @examples
#' rp_desc(iris$Sepal.Length)
#' rp_desc(iris$Sepal.Length, seq(0, 1, 0.10))
#' @export
#'
rp_desc <- function(x, pr = seq(0, 1, 0.25), round_to = 4) {

  # quick check: input must be numeric
  stopifnot(class(x) %in% c('integer', 'numeric'), all(pr >= 0 & pr <= 1))

  # defining the quantifies for the function
  nms <- paste0("p", round(pr * 100, 0))
  qnt <- rbind.data.frame(round(quantile(x, probs = pr, na.rm = T, names = F),
                                round_to))
  names(qnt) <- nms

  # returning output as a data.frame
  cbind.data.frame(
    n = length(x), NAs = sum(is.na(x)),
    avg = round(mean(x, na.rm = T), round_to),
    qnt, std = round(sd(x, na.rm = T), round_to))

}

