# ============================================================================ #
# Functions to compare: xlslx, text, R functions, ...
# Technically, these are simple wrapper functions to diffr::diffr()
# ============================================================================ #

#' Compares two text files
#' @name compare_text
#' @description
#' This is a wrapper of the function \code{diffr::diffr}. See there for details.
#'
#' @param file1,file2 Paths to text files
#' @param wordWrap,\dots passed to \code{\link{diffr}}
#'
#' @export
#' @importFrom diffr diffr
#'
compare_text <- function(file1, file2, wordWrap = FALSE, ...){
  stopifnot(file.exists(file1), file.exists(file2))
  diffr(file1, file2, wordWrap = wordWrap, ...)
}

#' Compares two tables from Excel files
#' @name compare_xlsx
#' @description
#' This function compares worksheets of the same name from two different Excel files.
#' Useful to find changes in configuration files between two releases
#'
#' @param xlsx1,xlsx2 String constants. Paths to Excel files
#' @param sheet String constant specifying the worksheets from the files to be compared. Default is the first sheet in Excel files.
#' @param sep Character constant specifying the character to be used as column separator in the output. Default is "|".
#' @param wordWrap,\dots passed to \code{\link{diffr}}
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
#' @importFrom diffr diffr
#' @importFrom openxlsx getSheetNames read.xlsx
#' @importFrom utils write.table
#'
compare_xlsx <- function(xlsx1, xlsx2, sheet = getSheetNames(xlsx1)[1], wordWrap = FALSE, ...,
                        sep = "|"){
  stopifnot(file.exists(xlsx1), file.exists(xlsx2),
            sheet %in% openxlsx::getSheetNames(xlsx1),
            sheet %in% openxlsx::getSheetNames(xlsx2))
  if (missing(sheet)) message("Comparing contents of sheet '", sheet, "'")
  utils::write.table(openxlsx::read.xlsx(xlsx1, sheet), file1 <- tempfile(), sep = sep)
  utils::write.table(openxlsx::read.xlsx(xlsx2, sheet), file2 <- tempfile(), sep = sep)
  compare_text(file1, file2, before = xlsx1, after = xlsx2, wordWrap = wordWrap, ...)
}

#' Compares two functions
#' @name compare_function
#' @description
#' Prints the two functions to temporary text files and compares these with \code{\link{compare_text}}.
#'
#' @param fun1,fun2 Paths to text files
#' @param wordWrap,\dots passed to \code{\link{diffr}}, via \code{\link{compare_text}}.
#'
#' @examples
#' \dontrun{
#' compare_functions(stats:::filter, dplyr::filter)
#' }
#'
#'
#' @export
compare_function <- function(fun1, fun2, wordWrap = FALSE, ...){
  stopifnot(is.function(fun1), is.function(fun2))
  fun1Name <- deparse(substitute(fun1))[[1]]
  fun2Name <- deparse(substitute(fun2))[[1]]
  message("Comparing functions ", fun1Name, " and ", fun2Name)
  txt1 <- capture.output(print(parse(text = deparse(fun1), keep.source = FALSE)[[1]]))
  txt1 <- gsub("    ", "  ", txt1)  # reduces indentation from 4 to 2 spaces
  write(txt1, file = (tmp1 <- tempfile()))
  txt2 <- capture.output(print(parse(text = deparse(fun2), keep.source = FALSE)[[1]]))
  txt2 <- gsub("    ", "  ", txt2)
  write(txt2, file = (tmp2 <- tempfile()))
  compare_text(tmp1, tmp2, before = fun1Name, after = fun2Name, wordWrap = wordWrap,
             ...)
}

#' Compare the contents of two RData-files
#' @name compare_rdata
#'
#' @param s1,s2 string constants indicating the file locations of two \code{RData} files to be compared
#' @param ignore.funs logical constant, if \code{TRUE} functions are ignored
#' @param check.attributes logical constant, if \code{TRUE} objects attributes are included in comparison with \code{all.equal}.
#' @return a list of class "compareRData".
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
#'
compare_rdata <- function(s1, s2, ignore.funs = TRUE, check.attributes = TRUE){
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

#' @describeIn compare_rdata
#' print method for class compareRData
#'
#' @param x An object of class compareRData as returned from function compareRData
#' @param \dots Any other arguments have currently no effect.
#'
#' @importFrom utils capture.output
#' @export
#'
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
