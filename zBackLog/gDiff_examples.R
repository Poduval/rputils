
#' Tools for easy comparison of two groups in a data frame.
#'
#' Tools are provided for fitting models discriminating between two groups, 
#' for visualizing them and for the identification of the variables contributing to the discrimination. 
#' Four methods from broadly used packages are available as modelling tools: 
#' \code{\link{ranger}} (the default), \code{\link{xgboost}}, \code{\link{lm}} and \code{\link{glm}}.
#'
#' @section How to use the package:
#' 
#' The typical workflow comprises the following steps. 
#' \enumerate{
#' \item{Specify your discrimination task in a \code{\link{gDiff}} object.}
#' \item{Fit a statistical model discrinating between the two groups with the function \code{\link{fit}}, 
#' thereby specifying the type of model in the parameter \code{method}.}
#' \item{Examine the quality of separation, using \code{print} and \code{plot} methods 
#' (the latter is currently only available in case you have chosen \code{method = "xgboost"}) and
#' the function \code{\link{separation}}, of which the result can be plotted, too.}
#' \item{Get the variable importance with \code{\link{importance}}. 
#' Again, the result has a \code{plot} method.}
#' \item{To see the distribution of the variables that have turned out to be important, 
#' use \code{\link{plot.gDiff}} with an appropriate \code{formula}.}
#' This will generate univarite or bivariate plots.
#' }
#' These steps are illustrated in the examples below.
#'
#' @docType package
#' @name gDiff-package
#' @aliases gDiff-package
#' 
#' @examples
#' # === EXAMPLE 1: Different gDiff's ===
#' g <- factor(rep(1:2, each = 75), labels = c("group1", "group2"))
#' gDiff(iris, g)
#'
#' # Transform a numeric variable to factor:
#' iris1 <- iris    
#' iris1$Sepal.Length <- factor(iris$Sepal.Length)
#' gDiff(iris1, g)
#' 
#' # data containing NA's:
#' iris2 <- iris
#' iris2[[1]][sample(150, 1)] <- 
#' iris2[[2]][sample(150, 10)] <- 
#' iris2[[3]][sample(150, 100)] <- 
#' iris2[[5]][sample(150, 10)] <- NA
#' gDiff(iris2, g)
#' 
#' # Column containing only NA's are dropped automatically:
#' iris3 <- iris
#' iris3[1] <- NA
#' gDiff(iris3, g)
#'  
#' 
#' # ===EXAMPLE 2: Numeric and character variables ===
#' g <- factor(rep(1:2, each = 75), labels = c("group1", "group2"))
#' 
#' x <- gDiff(iris, g)
#' x
#' 
#' # Fit model separating the two groups
#' xfit <- fit(x, fast = TRUE)
#' xfit
#' plot(xfit)
#' # Quality of separation?
#' head(sep <- separation(xfit))
#' summary(sep)
#' plot(sep)
#' # Which variables contribute to the separation?
#' imp <- importance(xfit)
#' plot(imp)
#' 
#' # --- plot.gDiff ---
#' plot(x, ~Petal.Width)                   # one numeric variable
#' plot(x, ~Petal.Width, discrete = TRUE)  # one numeric variable, discrete=TRUE
#' plot(x, ~Species)                       # one character variable / factor
#' 
#' plot(x, Petal.Length ~ Sepal.Width)  # Two numeric variables
#' plot(x, Petal.Length ~ Sepal.Width, smooth = FALSE)
#' 
#' plot(x, Petal.Length ~ Species)      # Numeric vs. factor
#' plot(x, Species ~ Petal.Length)      # Factor vs. numeric -> switching rhs & lhs
#' plot(x, Species ~ Species)           # Factor vs. factor
#' plot(x, cut(Sepal.Length, c(0, quantile(Sepal.Length))) ~ Species)
#' 
#' 
#' # ------------------------------------------------------------------------------
#' 
#' # === EXAMPLE 3: SEPARATION BECOMES VISIBLE IN BIVARIATE SPACE ONLY ===
#' library(mnormt)
#' d <- 10
#' n <- 1000
#' 
#' # random std normal data
#' RNGversion("3.4.4"); set.seed(123)
#' M <- matrix(rbinom(d*d, size = 1, prob = .5)*2-1, d)
#' X <- matrix(rnorm(d*n), n) %*% M
#' cor(X)
#' 
#' # Both groups identical -> no separation at all
#' xx <- gDiff(rbind(X, X), rep(1:2, each = n))
#' xx
#' (xfit <- fit(xx, fast = TRUE))
#' plot(xfit)
#' 
#' # Inverting X1 -> separation not in univariate considerations, but in bivariate
#' X1 <- X
#' X1[, 1] <- -X1[, 1]
#' xx <- gDiff(rbind(X, X1), rep(1:2, each = n))
#' xx
#' xfit <- fit(xx, fast = TRUE)
#' plot(xfit)
#' 
#' # Quality of separation?
#' head(sep <- separation(xfit))
#' summary(sep)
#' plot(sep)
#' # Which variables contribute to the separation?
#' imp <- importance(xfit)
#' plot(imp)
#' 
#' plot(xx, ~V1)      # Apparently no difference
#' plot(xx, ~V2)      # Apparently no difference
#' plot(xx, ~V9)      # Apparently no difference
#' plot(xx, V1 ~ V2)  # Difference becomes visible in bivariate plot
NULL

