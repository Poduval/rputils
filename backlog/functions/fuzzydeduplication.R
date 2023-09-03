
#' Cluster analysis based on data matrix X (helper function)
#' 
#' @param X Input data matrix
#' @param tol tolerance
#' 
#' @return Returns an integer vector defining the resulting clustering.
#' 
#' @examples
#' dat <- mtcars[, c(5, 9, 10)] ## Data matrix
#' get_cluster(dat)             ## Displays which object belogs to which cluster
#'                              ## clusters are creating based upon the euclidean distnce
#' get_cluster(dat, tol=0)      ## handling more efficently
#' table(get_cluster)           ## Each cluster has how many observations
#' 
#' @importFrom stats dist hclust cutree
#' @export
get_cluster <- function(X, tol = 0.01){
  if (nrow(X) == 1) {
    return(1L)
  }
  if (tol == 0){
    rows <- do.call(paste, c(X, sep = "\r"))
    return(as.integer(factor(rows, levels = unique(rows))))
  }
  ca <- hclust(dist(X)^2, method = "complete")
  cutree(ca, h = tol * ncol(X))
}

#' Identify group minima or group maxima (helper function)
#' 
#' @param x order variable
#' @param group group variable (of same length as \code{x})
#' @param max Logical: if \code{TRUE}, group maxima are looked up instead of minima
#' 
#' @return Returns an integer vector contining the positions of the group extrema.
#' 
#' @examples
#' with(mtcars, findGroupMin(mpg, gear))
#' # -> Returns an integer vector containing the positions of the group extrema
#' 
#' findGroupMin(mpg, gear, max=TRUE) 
#' # -> group maxima are looked up instead of minima
#' 
#' @export
findGroupMin <- function(x, group, max = FALSE){
  stopifnot(length(group) == length(x))
  ord <- order(x, decreasing = max, na.last = !max)
  ord[!duplicated(group[ord])]
}

#' Fuzzy deduplication
#' 
#' This is the main fuzzy deduplication function. 
#' To find appropriate cutoff values for the parameter \code{tol}, use the functions \code{\link{dissimilarities}} 
#' and \code{\link{dissimilar}}.
#' 
#' @details 
#' \code{NA}s allowed (but might not be smart if in exact field)
#' 
#' @param dat matrix, \code{data.frame} or list of vectors
#' @param fuzzy A character vector with names of normalized(!) numeric columns to be considered in fuzzy deduplication
#' @param exact A character vector with names identifying the groups in which the rows are compared
#' @param standardize Logical: If \code{TRUE}, then the fuzzy variables are divided by their sample standard deviation
#' @param select The name of the variable to be used to select the "non-duplicate" rows
#' @param max Logical: should max or min of "select" be chosen to select "non-duplicates"?
#' @param select.name Variable name giving duplicates (0) or non-duplicates(1)
#' @param tol Numeric tolerance (0 means only exact duplicates in "fuzzy" columns are identified as duplicates)
#' @param add Character vector of columns of \code{dat} to keep in the output, or alternatively \code{TRUE}, 
#' meaning all columns in \code{dat}.
#' @param show.diag If \code{TRUE}, some useful information on deduplication is shown
#' @param groupVarName Name of the grouping variable in the resulting \code{data.frame}.
#' 
#' @return A \code{data.frame} (with additional class attribute \dQuote{Fuzzydedup}),
#' having columns \code{groupVarName}  (indicating clusters of similar data lines 
#' w.r.t. numeric \code{fuzzy} variables within levels of \code{exact}), 
#' \code{select.name} (if not \code{NULL}) and the variables listed in \code{add}.
#' Output is in same row order as \code{dat}.
#' 
#' @seealso \code{\link{dissimilarities}}, \code{\link{plot.dissimilarities}}, 
#' \code{\link{dissimilar}}, \code{\link{getMaxDiss}}.
#' 
#' @examples 
#' library(dplyr)
#' library(ggplot2)
#' 
#' # Simulate data
#' {
#'   RNGversion("3.4.4")
#'   set.seed(98765)
#'   midpoints <- data.frame(xmid = runif(10), 
#'                           ymid = runif(10),
#'                           house = factor(LETTERS[1:10]),
#'                           town = factor(sample(1:3, 10, replace = TRUE)))
#'   d <- midpoints[rep(1:10, each = 5), ] %>% 
#'     mutate(x = xmid + rnorm(50, sd = 0.05),
#'            y = ymid + rnorm(50, sd = 0.05))
#'   d$xmid <- d$ymid <- NULL
#' } 
#' # Display data
#' plotmydata <- ggplot(d, aes(x, y, shape = house, color = town, fill = town)) + 
#'   geom_point(size = 3) +
#'   scale_shape_manual(values = 1:10)
#' plotmydata
#' # Assumptions: 
#' #   * rows in same 'house' groups refer to same actual house
#' #   * values of x and y are numeric variables describing the houses, values are only approximate
#' #   * 'house' is _not_ available to the user
#' #   * we know that obs from different towns can not refer to same house
#' 
#' # compute pairwise distances
#' distances <- dissimilarities(d, exact = "town", fuzzy = c("x", "y"),
#'                              standardize = TRUE)
#' nrow(distances)  # choose(nrow(d), 2)
#' car::some(distances)
#' 
#' # Plot histograms of 'inside' and 'across' distances
#' plot(distances)
#' plot(distances, vline = 0.7)  # Add reference lines at location cutting off 
#'                               # characteristic peak in 'inside' histogram
#' 
#' # Fuzzy deduplication
#' fdups <- find_duplicates(d, exact = "town", fuzzy = c("x", "y"),
#'                          standardize = TRUE, add = TRUE, tol = 0.7)
#' 
#' plot(fdups)  # same as: plot(distances, vline = getMaxDiss(fdups))
#' # Note: tol value and cutoff used on distances in cluster analysis are not identical
#' attr(fdups, "tol") # tol
#' getMaxDiss(fdups)  # cutoff value: equals sqrt(tol*length(fuzzy)), see ?getMaxDiss
#' 
#' print(addmargins(xtabs(~ house + New_ID, fdups)), 
#'       zero.print = ".")
#' # retry find_duplicates() with tol = 0.25 -> better
#' 
#' ggplot(fdups, aes(x, y, shape = New_ID, color = town)) + 
#'   geom_point(size = 3) +
#'   scale_shape_manual(values = 1:10)
#' # -> compare with: plotmydata
#' 
#' # Note: with bigger data, accelerate plotting by providing appropriate 'diss' arg:
#' plot(fdups, diss = distances)
#' 
#' # Show most dissimilar pairs of objects within same cluster:
#' dissimilar(fdups)  
#' dissimilar(fdups, diss = distances) # same output, but faster!
#' 
#' 
#' # ==== More on dissimilarities: ====
#' dist1 <- dissimilarities(d, exact = "house", fuzzy = c("x", "y"), 
#'                          standardize = FALSE)  # without standardizing
#' # Recalculate a distance:
#' i <- 987
#' dist1[i, ]
#' sqrt(sum((d[dist1$i1[i], c("x", "y")] - d[dist1$i2[i], c("x", "y")])^2))  # Euclidian distance
#' 
#' # Illustrating "inside" pairs and "across" pairs
#' car::some(dist1)
#' dist1 %>% 
#'   ggplot(aes(i1, i2)) + 
#'     geom_point()
#' .Last.value + facet_wrap(~type)
#' 
#' @importFrom stats sd
#' @importFrom utils head tail
#' @export
find_duplicates <- function(dat, fuzzy, exact = NULL, standardize = FALSE, 
                            select = NULL, max = TRUE, select.name = "Selected", 
                            tol = 0.01, add = NULL, show.diag = FALSE, 
  groupVarName = "New_ID"){
  stopifnot(length(fuzzy)>0, c(exact, fuzzy) %in% names(dat))
  if (length(exact) == 0) warning("Deduplication without 'exact' variable(s) rarely makes sense.")
  dat <- data.frame(dat)
  if (isTRUE(add)) add <- names(dat)
  if (nrow(dat) < 1) stop("\nNo line provided")
  missingFields <- setdiff(c(add, fuzzy, exact, select), colnames(dat))
  if (length(missingFields) > 0) stop(paste("\nMissing columns:", paste(missingFields, collapse = ", ")))
  if (length(intersect(fuzzy, exact))) stop("\n'fuzzy' variables and 'exact' variables overlap")
  if (!length(fuzzy)) stop("\nNo fuzzy variables specified")
  if (!all(sapply(dat[fuzzy], is.numeric))) stop("\nNon-numeric 'fuzzy' fields")
  if (anyNA(dat[fuzzy])) 
    warning("There are NAs in 'exact' variables. This may lead to inappropriate clustering.")

# Standardization of fields
  if (standardize) {
    sds <- apply(dat[fuzzy], 2L, sd, na.rm = TRUE)
  } else {
    sds <- rep(1, length(fuzzy))
  }
  Q <- sweep(dat[fuzzy], 2L, sds, FUN = "/")

# Run the cluster analyses within group
  if (is.null(exact)) {
    dat[[groupVarName]] <- get_cluster(Q, tol)
  } else if (tol == 0){
    rows <- do.call(paste, c(dat[c(exact, fuzzy)], sep = "\r"))
    dat[[groupVarName]] <- factor(rows, levels = unique(rows))
  } else {
    groupvar <- do.call(paste, c(dat[, exact, drop = FALSE], sep = "|"))
    nc <- ncol(Q)
    New_ID <- unsplit(
      lapply(split.default(unlist(Q, use.names = F, recursive = F), groupvar), 
             function(x) get_cluster(matrix(x, ncol = nc), tol = tol)), 
      groupvar)
    dat[[groupVarName]] <- paste(groupvar, New_ID, sep = "|")  # ist kein Faktor!
  }
  if (length(select) == 1) {
    stopifnot(select %in% names(dat))
    dat[, select.name] <- rep(0, nrow(dat))
    subs <- findGroupMin(dat[, select], dat[[groupVarName]], max)
    dat[subs, select.name] <- 1
    add <- c(add, select.name)
  }
  if (show.diag) {
    cat("\nProportion of fuzzy duplicates:", mean(duplicated(dat[[groupVarName]])) * 100, "%")
    cat("\nProportion of exact duplicates:", mean(duplicated(dat[c(fuzzy, exact)])) * 100, "%")
    cat("\n\nNames and sizes of largest clusters")
        largest.clusters <- rev(tail(sort(table(dat[[groupVarName]]))))
        print(largest.clusters)
        cat("\nFirst lines of largest cluster\n")
        printCols <- unique(c(add, fuzzy, exact, groupVarName))
        print(head(subset(dat[printCols], dat[[groupVarName]] == names(largest.clusters)[1])))
    cat("\nLargest standard deviations of fuzzy fields across clusters\n")
        print(sqrt(apply(
          gvar(dat[fuzzy], dat[[groupVarName]], reorder = FALSE, na.rm = TRUE),
                    2, max, na.rm = TRUE)))
        cat("\n\n")
  }
  out <- dat[, c(add, groupVarName), drop = FALSE]
  structure(out, 
    class = c("Fuzzydedup", "data.frame"),
    exact = exact, fuzzy = fuzzy, tol = tol, 
    standardize = standardize, groupVarName = groupVarName, 
    call = match.call(), environment = parent.frame())
}


#' @describeIn find_duplicates 
#' The \code{plot} method for class \dQuote{Fuzzydedup} essentially executes \code{plot(\link{dissimilarities}(x))} (see \code{\link{plot.dissimilarities}}). Providing a \code{diss} object can significantly reduce execution time - thereby, the inputs \code{x} and \code{diss} must match (i.e. have been generated with the same data and the same values of \code{exact}, \code{fuzzy}, \code{standardize}).
#' 
#' @param x A \dQuote{Fuzzydedup} object (see \code{\link{find_duplicates}}.
#' @param nmax passed to \code{dissimilarities}.
#' @param bins,\dots passed to \code{plot.dissimilarities}.
#' @param diss A \code{\link{dissimilarities}} object.
#' 
#' @importFrom graphics plot
#' @export
plot.Fuzzydedup <- function(x, nmax = 5000, bins = 60, ..., 
                            diss = dissimilarities(x, nmax = nmax)){
  plot(diss, vline = getMaxDiss(x), bins = bins, ...)
}  
