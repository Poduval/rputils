#' Read object definition from R Script, list names of all functions defined in RScript
#' 
#' @param file Character string: file path and name. Default: active RStudio script (won't work in RGUI!)
#' @param objects Character string: name of one or several objects to be read from script.
#' Default: all functions that are defined in \code{file}.
#' @param show Logical: print object to console?
#' @param \dots Passed to \code{\link{parse}}.
#' @param x Character string: file path and name. Default: active RStudio script (won't work in RGUI!)
#' 
#' 
#' @name RScriptFuns
#' 
NULL




#' @describeIn RScriptFuns
#' Read object definition from R script
#' 
#' All code lines of pattern \code{name <- anyDefinition} or \code{replacementFun(name) <- anything} 
#' will be read from the \code{file} and executed.
#' 
#' Note that this function isn't very general: 
#' Statements as \code{(name <- anyDefinition)} (assignment enclosed in parentheses) 
#' or \code{assign(name, ...)} will be missed.
#' 
#' Returns \code{NULL} invisibly, but function is generally called for its side effect of
#' assigning objects in the current environment.
#' @importFrom rstudioapi getSourceEditorContext
#' 
#' @export
fromRScript <- function(file = rstudioapi::getSourceEditorContext()$path, 
                        objects = allFunctions(parsed_code), show = TRUE, ...){
  stopifnot(file.exists(file), length(file) == 1)
  parsed_code <- as.list(parse(file, ...))
  for (obj in objects){
    attempt <- try(.fromRScript_oneObj(parsed_code, object = obj, show = show, ...),
                   silent = TRUE)
    if (!inherits(attempt,"try-error")){
      obj_read <- attempt
      if (is.function(obj_read)) environment(obj_read) <- parent.frame()
      assign(obj, obj_read, envir = parent.frame())
    }
  }
  invisible()
}

# Processes only one object
.fromRScript_oneObj <- function(parsed, object, show = TRUE, ...){
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


#' @describeIn RScriptFuns
#' Lists the names of all functions that are defined in a script
#' Same limitations as for \code{\link{fromRScript}} also apply here.
#' @importFrom rstudioapi getSourceEditorContext
#' 
#' @export
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

