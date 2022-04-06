
# Utility functions to compare simlilar (but not identical) text files or sheets of Excel files
# ---------------------------------------------------------------------------------------------
# May be helpful if you want to compare the Excel config files from two model releases
# Technically, these are simple wrapper functions to diffr::diffr()

#' Compares two text files
#' @description 
#' This is a wrapper of the function \code{diffr::diffr}. See there for details.
#' 
#' @param file1,file2 Paths to text files
#' @param wordWrap,\dots passed to \code{\link{diffr}}
#' 
#' @export
#' @importFrom diffr diffr
compareTxt <- function(file1, file2, wordWrap = FALSE, ...){
  stopifnot(file.exists(file1), file.exists(file2))
  diffr(file1, file2, wordWrap = wordWrap, ...)
}

#' Compares two tables from Excel files
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
#' setwd("../model.paidrentR1.ch")
#' xl1 <- "4_Implementation/18.1.1/configurationPRR1.xlsx"
#' xl2 <- "4_Implementation/19.1.0/configurationPRR1.xlsx"
#' 
#' compareXlsx(xl1, xl2)  # Taking first sheet by default 
#' compareXlsx(xl1, xl2, "validation")
#' 
#' # dots are passed to diffr
#' compareXlsx(xl1, xl2, "validation", minJumpSize = 0, contextSize = 0)
#' 
#' }
#' 
#' @export
#' @importFrom diffr diffr
#' @importFrom openxlsx getSheetNames read.xlsx
#' @importFrom utils write.table
compareXlsx <- function(xlsx1, xlsx2, sheet = getSheetNames(xlsx1)[1], wordWrap = FALSE, ...,
                        sep = "|"){
  stopifnot(file.exists(xlsx1), file.exists(xlsx2), 
            sheet %in% openxlsx::getSheetNames(xlsx1), sheet %in% openxlsx::getSheetNames(xlsx2))
  if (missing(sheet)) message("Comparing contents of sheet '", sheet, "'")
  utils::write.table(openxlsx::read.xlsx(xlsx1, sheet), file1 <- tempfile(), sep = sep)
  utils::write.table(openxlsx::read.xlsx(xlsx2, sheet), file2 <- tempfile(), sep = sep)
  compareTxt(file1, file2, before = xlsx1, after = xlsx2, wordWrap = wordWrap, ...)
}


#' Compares two functions
#' @description 
#' Prints the two functions to temporary text files and compares these with \code{\link{compareTxt}}.
#' 
#' @param fun1,fun2 Paths to text files
#' @param wordWrap,\dots passed to \code{\link{diffr}}, via \code{\link{compareTxt}}.
#' 
#' @examples
#' compareFuns(stats:::predict.lm, modelling:::predict.llm)
#' 
#' @export
compareFuns <- function(fun1, fun2, wordWrap = FALSE, ...){
  stopifnot(is.function(fun1), is.function(fun2))
  fun1Name <- deparse(substitute(fun1))[[1]]
  fun2Name <- deparse(substitute(fun2))[[1]]
  message("Comparing functions ", fun1Name, " and ", fun2Name)
  txt1 <- capture.output(print(parse(text = deparse(fun1), keep.source = FALSE)[[1]]))
  txt1 <- gsub("    ", "  ", txt1)  # reduces identation from 4 to 2 spaces
  write(txt1, file = (tmp1 <- tempfile()))
  txt2 <- capture.output(print(parse(text = deparse(fun2), keep.source = FALSE)[[1]]))
  txt2 <- gsub("    ", "  ", txt2)
  write(txt2, file = (tmp2 <- tempfile()))
  compareTxt(tmp1, tmp2, before = fun1Name, after = fun2Name, wordWrap = wordWrap, 
             ...)
}



