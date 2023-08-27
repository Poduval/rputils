# fromRData.R


#' Read one or several R objects from a RData file
#' 
#' @param file A \code{.RData} file.
#' @param objName Name(s) of R objects that are saved in \code{file}. 
#' If \code{objName} is missing, \code{multiple} is set to \code{TRUE} and all objects are read from the file.
#' @param multiple Logical: Multiple elements allowed? Must be \code{TRUE} if \code{length(objName) > 1}.
#'
#' @details The function has a built-in buffering mechanism that avoids loading the same .RData-file 
#' multiple times in one R-session. 
#' If the file has been modified since last loading, it will be loaded again.
#'   
#' @return If \code{multiple = TRUE} the R object specified in \code{objName}; 
#' otherwise a named list of R objects.
#' 
#' @examples 
#' \dontrun{
#' fileLoc <- "H:/4_PRCHR1_2/20.1.0/RData/31_ac.RData"
#' l1 <- fromRData(fileLoc, "sigma_ac")
#' class(l1)
#' fileLoc <- "H:/4_PRCHR1_2/20.1.0/RData/30_fullmodel.RData"
#' l2 <- fromRData(fileLoc, c("regional", "pminmax"), multiple = T )
#' class(l2)
#' loadedFromRData()
#' }
#' @export
fromRData <- function(file, objName, multiple = FALSE){
  file <- normalizePath(file, "/")
  fileExists <- file.exists(file)
  # get option "._loadedFromRData_."
  .loadedFromRData. <- getOption("._loadedFromRData_.")
  if (is.null(.loadedFromRData.)){
    .loadedFromRData. <- new.env(parent = emptyenv())
  }
  # Reset option "._loadedFromRData_." when leaving the function
  on.exit(options(`._loadedFromRData_.`= .loadedFromRData.))
  # Check if file has been loaded earlier?
  fileAlreadyloaded <- (file %in% names(.loadedFromRData.))
  if (fileAlreadyloaded && !fileExists){
    .loadedFromRData.[[file]] <- NULL
    fileAlreadyloaded <- FALSE
  }
  if (!fileExists) stop("File does not exists: ", file)
  # If file has been modified since loading: set fileAlreadyloaded <- FALSE
  if (fileAlreadyloaded && 
      file.mtime(file) > attr(.loadedFromRData.[[file]], "loadingTime")){
    fileAlreadyloaded <- FALSE
  }
  # Load file, if not already loaded
  if (!fileAlreadyloaded){
    tmpenv <- new.env()
    load(file, envir = tmpenv)
    if (getOption("verbose")) message("Loading file: ", file)
    attr(tmpenv, "loadingTime") <- Sys.time()
    .loadedFromRData.[[file]] <- tmpenv
  }
  # get object(s)
  if (missing(objName)){
    objName <- ls(.loadedFromRData.[[file]], all.names = TRUE)
    multiple <- TRUE
  }
  stopifnot(exists(objName, envir = .loadedFromRData.[[file]]),
            length(objName) == 1L || multiple)
  if (getOption("verbose")) message("Extracting from ", file, ":\n  ", paste(objName, collapse = ", "))
  if (multiple){
    mget(objName, envir = .loadedFromRData.[[file]], inherits = FALSE)
  } else {
    get(objName, envir = .loadedFromRData.[[file]], inherits = FALSE)
  }
}

# loadedFromRData(): no examples yet...
# @examples
# loadedFromRData()
# loadedFromRData(details = TRUE)

#' Returns environment with all RData files previously loaded with fromRData
#' @description 
#' List RData files that have been loaded
#' 
#' @name loadedFromRData
#' 
#' @param details Logical constant. If \code{TRUE} list of object names for each file is displayed
#' @return data frame with file-name, loading time, number of objects, and, if \code{details=TRUE}, object names. 
#'  
#' @export
loadedFromRData <- function(details = FALSE){
  l <- as.list(getOption("._loadedFromRData_."))
  d <- data.frame(file = as.character(names(l)),
                  loading.time = format(as.POSIXct(vapply(l, attr, "loadingTime", FUN.VALUE = 0), 
                                                   origin = "1970-01-01"),
                                        "%Y-%m-%d %H:%M:%S"),
                  n.objects = lengths(sapply(l, ls, all.names = TRUE, simplify = FALSE), use.names = FALSE))
  if (details)
    d$objects <- vapply(l, function(x) paste(ls(x, all.names = TRUE), 
                                             collapse = ", "), character(1))
  class(d) <- c("loadedFromRData", "data.frame")
  d
}


#' @describeIn loadedFromRData
#' Printing method for objects of class \code{loadedFromRData}
#' @param x An object of class 'loadedFromRData' as returned from \code{loadedFromRData}
#' @param \dots More arguments forwarded to \code{print.data.frame}
#' @param row.names logical constant, if \code{TRUE} row names are printed (as in print.data.frame)
#' @param right logical constant, indicating whether or not strings should be right-aligned. The default is left-alignment. 
#' @export
print.loadedFromRData <- function(x, ..., row.names = FALSE, right = FALSE){
  print.data.frame(x, ..., row.names = row.names, right = right)
}



#' Compare the contents of two RData-files
#'  
#' @param s1,s2 string constants indicating the file locations of two \code{RData} files to be compared
#' @param ignore.funs logical constant, if \code{TRUE} functions are ignored
#' @param check.attributes logical constant, if \code{TRUE} objects attributes are included in comparison with \code{all.equal}.
#' @return a list of class "compareRData". 
#' 
#' @name compareRData
#' 
#' @examples
#' \dontrun{
#' compareRData("H:/4_PRCHR3_2/20.1.0/RData/20_prepdata.RData",
#'              "H:/4_PRCHR3_2/19.1.0/RData/20_prepdata.RData")
#'               
#' } 
#' 
#' @export 
#' 
compareRData <- function(s1, s2, ignore.funs = TRUE, check.attributes = TRUE){
  load(s1, (xList <- new.env()))
  xList <- as.list(xList)
  load(s2, (yList <- new.env()))
  yList <- as.list(yList)
  if (ignore.funs){
    xList <- xList[!sapply(xList, is.function)]
    yList <- yList[!sapply(yList, is.function)]
  }
  allnames <- union(names(xList), names(yList))
  inboth  <- intersect(names(xList), names(yList))
  compare <- mapply(all.equal, xList[inboth], yList[inboth], 
                    MoreArgs = list(check.attributes = check.attributes),
                    USE.NAMES = TRUE, SIMPLIFY = FALSE)
  x.only <- setdiff(names(xList), inboth)
  y.only <- setdiff(names(yList), inboth)
  structure(list(compare = compare, x.only = x.only, y.only = y.only,
                 x = s1, y = s2, call = call),
            class = "compareRData")
}




#' @describeIn compareRData  
#' print method for class compareRData
#' 
#' @param x An object of class compareRData as returned from function compareRData
#' @param \dots Any other arguments have currently no effect. 
#' 
#' @importFrom utils capture.output
#' @export
print.compareRData <- function(x, ...){
  cat("'compareRData'\n",
      "  file 'x': ", x$x, "\n",   
      "  file 'y': ", x$y, "\n")
  ident <- vapply(x$compare, isTRUE, logical(1))
  if (any(ident)){
    cat("\n  === Identical in x and y: ===\n",
        paste0("    ", names(ident)[ident], collapse = "\n"), 
        "\n", sep = "")
    
  }
  if (length(x$x.only)){
    cat("\n  === Only in x: ===\n",
        paste0("    ", x$x.only, collapse = "\n"), 
        "\n", sep = "")
  }
  if (length(x$y.only)){
    cat("\n  === Only in y:===\n",
        paste0("    ", x$y.only, collapse = "\n"), 
        "\n", sep = "")
  }
  if (any(!ident)){
    cat("\n  === Different in x and y (as reported by 'all.equal(...)'): ===\n",
        paste0("    ", utils::capture.output(print(x$compare[!ident])),
               collapse = "\n"),
        "\n", sep = "")
  }
  invisible(x)
}



