# ============================================================================ #
# Functions to Read R functions from a R script
# ============================================================================ #

#' @describeIn Read object definition from R script
#' @rdname read_rscript
#' @name read_rscript
#'
#' @param file path of the rscript file
#' @param objects r object to be loaded, by default it loads all available ones
#' @param show logical, to print the function in the console or not
#' @param ... additional parameters to be passed to \code{.read_rscript_one_obj}
#'
#' @importFrom rstudioapi getSourceEditorContext
#'
#' @export
#'
read_rscript <- function(file = rstudioapi::getSourceEditorContext()$path,
                        objects = allFunctions(parsed_code), show = TRUE, ...){
  stopifnot(file.exists(file), length(file) == 1)
  parsed_code <- as.list(parse(file, ...))
  for (obj in objects){
    attempt <- try(.read_rscript_one_obj(parsed_code, object = obj, show = show, ...),
                   silent = TRUE)
    if (!inherits(attempt,"try-error")){
      obj_read <- attempt
      if (is.function(obj_read)) environment(obj_read) <- parent.frame()
      assign(obj, obj_read, envir = parent.frame())
    }
  }
  invisible()
}

#' @describeIn Processes only one object
#' @name .read_rscript_one_obj
#'
#' @param parsed parse text of the function, c
#' @param object r object
#' @param show logical, to print the function in the console or not
#'
#'
.read_rscript_one_obj <- function(parsed, object, show = TRUE){
  stopifnot(length(object) == 1)
  # select assignments
  i <- sapply(parsed,
              function(x) length(x) == 3 &&
                identical(as.character(x[[1]]), "<-") &&
                (object %in% all.vars(x[[2]])))
  wh <- which(i)
  if (length(wh) > 1) message("Found ", length(wh), " lines defining an object ", sQuote(object))
  if (length(wh) == 0){
    warning("Could not read object from file: ", object, call. = FALSE)
    invisible(NULL)
  }
  out <- parsed[wh]
  if (show){
    lapply(out, print)
  }
  for (codeLine in out)
    eval.parent(codeLine)
  invisible(get(object, envir = parent.frame()))
}

#' @describeIn
#' Lists the names of all functions that are defined in a script
#' Same limitations as for \code{\link{read_rscript}} also apply here.
#' @name allFunctions
#'
#' @param x file path of the r script
#'
#' @importFrom rstudioapi getSourceEditorContext
#' @export
#'
allFunctions <- function(x = rstudioapi::getSourceEditorContext()$path,
                         ...){
  if (is.character(x)){
    stopifnot(file.exists(x), length(x) == 1)
    x <- as.list(parse(x, ...))
  }
  i <- sapply(x,
              function(x) length(x) == 3 &&
                identical(as.character(x[[1]]), "<-") &&
                identical(x[[3]][[1]], as.name("function")))
  sapply(x[i], function(x) as.character(x[[2]]))
}

