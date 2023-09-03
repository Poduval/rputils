# ============================================================================ #
# Execute an R & Rmd scripts from terminal in batches.
# Useful for putting R into production
# ============================================================================ #

#' Execute an R script from shell
#'
#' @description
#' Executes an R script in batch mode.
#' The R code in \code{file} is executed in an independent R session, and
#' the console output (including the executed code) is stored in a text file
#' \code{file.path(destdir, outfile)}.
#'
#' @details
#' To avoid that critical warnings remain undetected, \code{outfile}
#' is scanned for lines beginning with the string \dQuote{Warning} after its generation,
#' and a warning is issued if there is at least one such line.
#' (Note that this may fail if your R session is using translations.)
#'
#' @param file Path and name of an R script to be executed.
#' @param destdir Output directory for .Rout-file (R console).
#' @param outfile Name of output .Rout file (R console), NOT including folder name - use \code{destdir} for that!
#' @param timing Logical: print execution time in console?
#'
#' @return Character string: path to the output file.
#'
#' @seealso \code{\link{Rmdbatch}}
#'
#' @export
#'
Rbatch <- function(file, destdir = ".",
                   outfile = paste0(basename(file), ".Rout"), timing = TRUE){
  # Check input
  stopifnot(length(file) == 1, file.exists(file), length(outfile) == 1)
  if (basename(outfile) != outfile){
    stop("Invalid ", sQuote("outfile"), " argument, expecting only a name. ",
         "Specify Rbatch(..., outfile=\"fileNAME\", destdir=\"fileLOCATION\")")
  }
  outfile <- normalizePath(file.path(destdir, outfile), mustWork = FALSE)
  file <- normalizePath(file)
  shellcmd <- paste0(normalizePath(file.path(R.home("bin"), "R"), mustWork = FALSE),
                    'CMD BATCH --vanilla', ' "', file, '" "', outfile, '"')
  cat("Processing R script:", basename(file), "...")
  shelltime <- system.time(shell(shellcmd, wait = TRUE))
  if (timing){
    cat("\rTiming of command line execution (", basename(file), "):\n", sep = "")
    print(shelltime)
    } else cat("\r")
  if (any(grepl("^Warning", readLines(outfile))))
    warning("Output file may contain warnings... check!")
  if (file.exists("Rplots.pdf")){
    file.rename("Rplots.pdf", sub("\\.Rout", ".Rplots.pdf", outfile))
  }
  invisible(outfile)
}

#' Execute an Rmd (Rmarkdown) script from shell
#'
#' @description
#' \code{Rmdbatch()} essentially creates a temporary R script executing
#' \code{rmarkdown::render(file, ...)}, which is then executed with \code{\link{Rbatch}()}.
#' It is assumed that your Rmarkdown script creates html output.
#'
#' @details
#' \code{Rmdbatch} produces \emph{two} output files:
#' A log file (contents that would be written to the R console) and a html report:
#' \itemize{
#' \item{
#' To specify the \strong{log file}, use the parameters \code{destdir} (default: current working directory) and
#' \code{outfile} (default: name of input \code{file} with \code{.Rout} appended).}
#' \item{
#' To specify the \strong{html file}, use the parameters \code{destdir.html} (default: directory of input \code{file})
#' and \code{outfile.html} (default: name of input \code{file}, but with extension \code{.html} instead of \code{.Rmd}).}
#' }
#'
#' @return Character string: path to the output \emph{log} file.
#'
#' @inheritParams Rbatch
#' @param file Path and name of an Rmarkdown script to be executed. Must have extension \code{.Rmd}.
#' @param destdir.html Where to move the html output file (assuming knitr is generating html output)
#' @param outfile.html Name of the output html file,
#' NOT including folder name - use \code{destdir.html} for that!
#' @param overwrite.html Logical: Whether to overwrite an existing html file in folder \code{destdir.html}.
#' @param \dots Passed to \code{rmarkdown::render()}.
#'
#' @seealso \code{\link{Rbatch}}
#'
#' @export
Rmdbatch <- function(file, destdir = ".",
                     outfile = paste0(basename(file), ".Rout"),
                     timing = TRUE,
                     destdir.html = dirname(file),
                     outfile.html = sub("Rmd$", "html", file),
                     overwrite.html = TRUE,
                     ...){
  # Check input
  if (!grepl("\\.Rmd$", file))
    stopifnot("Rmdbatch() expects an input file having extension ", sQuote(".Rmd"))
  stopifnot(length(file) == 1, file.exists(file), length(outfile) == 1,
            length(outfile.html) == 1)
  if (basename(outfile) != outfile){
    stop("Invalid ", sQuote("outfile"), " argument, expecting only a name. ",
         "Specify Rmdbatch(..., outfile=\"fileNAME\", destdir=\"fileLOCATION\")")
  }
  if (!missing(outfile.html) && (basename(outfile.html) != outfile.html)){
    stop("Invalid ", sQuote("outfile.html"), " argument, expecting only a name. ",
         "Specify Rmdbatch(..., outfile.html=\"fileNAME\", destdir.html=\"fileLOCATION\")")
  }

  # Generate temporary .R script
  Rtmpfile <- tempfile(fileext = ".R")
  cl <- parse(text = paste0("rmarkdown::render(\"",
                            normalizePath(file, "/"),
                            "\")"))[[1]]
  cl <- as.call(c(as.list(cl), list(...)))
  write(deparse(cl), file = Rtmpfile)
  # Run with Rbatch and save output
  Rbatch(Rtmpfile, destdir = destdir, outfile = outfile, timing = timing)
  htmlFile <- sub("Rmd$", "html", file)
  if (normalizePath(destdir.html) != normalizePath(dirname(file)) ||
      outfile.html != htmlFile){
    if (!file.exists(htmlFile)){
      warning("html output file ", sQuote(basename(htmlFile)), " not found in ",
              sQuote(normalizePath(dirname(file), "/")), call. = FALSE)
    } else {
      copy.ok <- file.copy(htmlFile,
                           file.path(destdir.html, basename(outfile.html)),
                           overwrite = overwrite.html)
      if (copy.ok){
        file.remove(htmlFile)
        message("html output file ", sQuote(basename(htmlFile)),
                " moved/renamed to ",
                sQuote(normalizePath(file.path(destdir.html, basename(outfile.html)), "/")))
      } else {
        warning("Could not move html output file ", sQuote(basename(htmlFile)), " to ",
                sQuote(normalizePath(file.path(destdir.html, basename(outfile.html)), "/")),
                call. = FALSE)
      }
    }
  }
  invisible(outfile)
}


# Show help for R CMD BATCH
# shell('R CMD BATCH --help')
