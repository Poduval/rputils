# ============================================================================ #
# Functions to Read elements from RData
# ============================================================================ #

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
#' }
#' @export
#'
read_rdata <- function(file, objName, multiple = FALSE) {

  file <- normalizePath(file, "/")
  fileExists <- file.exists(file)
  .loaded_rdata. <- getOption("._loaded_rdata_.")
  if (is.null(.loaded_rdata.)){
    .loaded_rdata. <- new.env(parent = emptyenv())
  }
  on.exit(options(`._loaded_rdata_.`= .loaded_rdata.))

  # Check if file has been loaded earlier?
  fileAlreadyloaded <- (file %in% names(.loaded_rdata.))
  if (fileAlreadyloaded && !fileExists){
    .loaded_rdata.[[file]] <- NULL
    fileAlreadyloaded <- FALSE
  }
  if (!fileExists) stop("File does not exists: ", file)

  # If file has been modified since loading: set fileAlreadyloaded <- FALSE
  if (fileAlreadyloaded &&
      file.mtime(file) > attr(.loaded_rdata.[[file]], "loadingTime")){
    fileAlreadyloaded <- FALSE
  }

  # Load file, if not already loaded
  if (!fileAlreadyloaded){
    tmpenv <- new.env()
    load(file, envir = tmpenv)
    if (getOption("verbose")) message("Loading file: ", file)
    attr(tmpenv, "loadingTime") <- Sys.time()
    .loaded_rdata.[[file]] <- tmpenv
  }

  # get object(s)
  if (missing(objName)){
    objName <- ls(.loaded_rdata.[[file]], all.names = TRUE)
    multiple <- TRUE
  }
  stopifnot(all(sapply(objName, exists, .loaded_rdata.[[file]])),
            length(objName) == 1L || multiple)

  if (getOption("verbose"))
    message("Extracting from ", file, ":\n  ", paste(objName, collapse = ", "))

  if (multiple){
    mget(objName, envir = .loaded_rdata.[[file]], inherits = FALSE)
  } else {
    get(objName, envir = .loaded_rdata.[[file]], inherits = FALSE)
  }
}

#' Returns environment with all RData files previously loaded with read_rdata
#' @name loaded_rdata
#' @description List RData files that have been loaded
#'
#' @param details Logical constant. If \code{TRUE} list of object names for each file is displayed
#' @return data frame with file-name, loading time, number of objects, and, if \code{details=TRUE}, object names.
#'
#' @examples
#' \dontrun{
#' loaded_rdata()
#' loaded_rdata(details = TRUE)
#' }
#'
#' @export
#'
loaded_rdata <- function(details = FALSE) {

  l <- as.list(getOption("._loaded_rdata_."))
  d <- data.frame(
    file = as.character(names(l)),
    loading.time = format(as.POSIXct(vapply(
      l, attr, "loadingTime", FUN.VALUE = 0),
      origin = "1970-01-01"), "%Y-%m-%d %H:%M:%S"),
    n.objects = lengths(sapply(
      l, ls, all.names = TRUE, simplify = FALSE), use.names = FALSE))

  if (details) {
     d$objects <- vapply(
       l, function(x) paste(ls(x, all.names = TRUE), collapse = ", "),
       character(1))
  }
  class(d) <- c("loaded_rdata", "data.frame")
  d
}

#' @describeIn loaded_rdata
#' Printing method for objects of class \code{loaded_rdata}
#' @param x An object of class 'loaded_rdata' as returned from \code{loaded_rdata}
#' @param \dots More arguments forwarded to \code{print.data.frame}
#' @param row.names logical constant, if \code{TRUE} row names are printed (as in print.data.frame)
#' @param right logical constant, indicating whether or not strings should be right-aligned. The default is left-alignment.
#' @export
print.loaded_rdata <- function(x, ..., row.names = FALSE, right = FALSE){
  print.data.frame(x, ..., row.names = row.names, right = right)
}




