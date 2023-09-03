
#' Find a subset of data generating a complete distance matrix
#' 
#' Given data \code{df} containing \code{NA}s, \code{keepRows()} finds a subset of 
#' this data such that a distance matrix (calculated with \code{\link{dist}}) 
#' of that subset will be free of \code{NA}s. 
#' This stepwise procedure probably won't always find the absolute optimum (largest possible subset), 
#' but the results proved reasonable and useful in practice.
#' 
#' @param df A \code{data.frame}.
#' 
#' @return Integer vector of selected rows.
#' 
#' @importFrom utils combn
#' @examples
#' # Example 1
#' df <- data.frame(
#'   roomNb = c(4.5, 4.5, 4.5, 4.5, 4.5, 4.5, NA, NA),  
#'   surfaceLiving = c(85, 85, 85, 85, NA, 85, NA, NA),  
#'   age = c(67, NA, NA, 67, 8, 67, 67, 67))
#' df
#' keepRows(df)
#' 
#' dist(df)
#' dist(df[keepRows(df), ])
#' 
#' 
#' # 2nd example: simulated data with predetermined proportion of NAs
#' probNA <- .5  # proportion of NAs
#' sparseData <- matrix(runif(1000), 100)
#' sparseData[sample(length(sparseData), round(probNA*length(sparseData)))] <- NA
#' 
#' # Proportion of NA distances
#' mean(is.na(dist(sparseData)))
#'   
#' kr <- keepRows(sparseData)
#' length(kr)/nrow(sparseData)   # Proportion of data kept
#' anyNA(dist(sparseData[kr, ])) # must be FALSE
#' mean(is.na(sparseData[kr, ])) # Proportion of NAs in kept data
#' 
#' @export
keepRows <- function(df){
  # Output: rows to be kept in data
  out <- seq(nrow(df))
  
  # Identify NAs by field
  nas <- nasInt <- is.na(df)
  
  # Create NA-pattern (strings consisting of '0's (=available) and '1's (=NA)
  mode(nasInt) <- "integer"
  patternID <- do.call(paste0, split(nasInt, col(nas)))
  patternID <- factor(patternID, levels = unique(patternID))
  # Note (important!): order of levels = order of appearance in df
  
  # Shortcut if only one pattern:
  if (nlevels(patternID) <= 1) return(out)

  # Identify pairs of patterns that will cause NA distance
  unas <- nas[!duplicated(patternID), , drop = FALSE]
  rownames(unas) <- levels(patternID)
  patternLogi <- setNames(split(unas, row(unas)), levels(patternID))
  NAdist <- setNames(data.frame(t(combn(levels(patternID), 2))), paste0("pattern", 1:2))
  NAdist$commonValues <- combn(patternLogi, 2, FUN = function(x) sum(!x[[1]] & !x[[2]]), 
                               simplify = TRUE)
  
  # Shortcut if no NA distances:
  if (all(NAdist$commonValues>0)) return(out)

  NAdist <- NAdist[NAdist$commonValues == 0, , drop = FALSE]   # commonValues=0 => distance will be NA
  
  # Identify 'information loss' resulting from deleting a given NA pattern from data
  NAloss <- data.frame(pattern = levels(patternID), 
                       freqs = setNames(tabulate(patternID, nbins = nlevels(patternID)), 
                                        levels(patternID)),
                       nValues = sapply(patternLogi, function(x) sum(!x)))
  NAloss$loss <- with(NAloss, freqs*nValues)

  # Repeatedly eliminate patterns until dist-matrix will be complete
  repeat {
    # Find the pattern involved in a NA-dist pair with minimum loss
    involvedPatterns <- as.character(unique(unlist(NAdist[c("pattern1", "pattern2")], 
                                                   use.names = FALSE)))
    wmin <- which.min(NAloss[involvedPatterns, , drop = FALSE]$loss)
    rmPattern <- involvedPatterns[wmin]
    rmRows <- which(patternID == rmPattern)
    out <- setdiff(out, rmRows)
    
    # Adjust NAdist
    rmFromNAdist <- with(NAdist, pattern1 == rmPattern | pattern2 == rmPattern)
    NAdist <- NAdist[!rmFromNAdist, , drop = FALSE]
    if (nrow(NAdist) == 0) break
  }
  # output
  out
}
