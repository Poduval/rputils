
#' checkConfig
#' 
#' @description 
#' Prepare the standardized R environment of a smr project.
#' In case of problems, see also the troubleshooting page in IAZI-Pedia: 
#' \url{http://iazipedia.iazi.local:8090/display/DEV/smrSetup}.
#' 
#' @param model.release Chracter string: The current model's release number.
#' @param project.home Home directory of the smr project.
#' @param data.dir Path to the directory where release-specific material is stored. 
#' Can be either an absolute path or a path relative to \code{project.home}.
#' @param Rversion The prescribed base R version required for the project.
#' @param releaseFile The projects's release file.
#' @param repos Repository path, defaults to \code{"W:/0_Config/R_Release"}.
#' @param zipLib Path to the directory with the zip files containing precompiled versions
#' of the package versions to be used in this project.
#' @param Rlibrary Path to R library where the packages are kept locally.
#' @param checkInstallation Logical: If \code{TRUE}, \code{\link{smrCheckPkgs}} will be executed, checking
#' if the installed packages are identical with those in \code{zipLib}. 
#' @param showInstallation Logical: If \code{TRUE}, information on packages not corresponding to those 
#' in \code{zipLib} will be printed. Note that the packages are not updated if 
#' \code{showInstallation=TRUE} (unless you also set \code{updatePackages=TRUE}).
#' @param updatePackages Logical: If \code{TRUE}, the local package installation will be synchronized with 
#' \code{zipLib}.
#' @param dontUpdate Either a numeric value or a character vector. See details section. 
#' @param ignoreFreezing Logical: Allow working on a project release that has been closed already?
#' @param createAllDirs Logical: If \code{TRUE}, all of the usual subdirectories of \code{project.home}
#' (\code{1_Management}, \code{2_Specification}, \code{3_Design}, \code{4_Implementation}, 
#' \code{5_Release}, \code{6_Test}, \code{7_Documentation}, \code{8_Delivery})
#' will be created, if they do not yet exist. If \code{FALSE}, only \code{"4_Implementation"} is required.
#' @param silent Logical: If \code{TRUE}, any output to the console will be suppressed.
#' 
#' @details 
#' If a smr project is opened in its default environment, \code{checkConfig()} can be run
#' without arguments. 
#' 
#' \code{checkConfig()} executes a number of checks and sets up a standardized R environment, 
#' so that identical versions of base R and all add-on packages are used by any user 
#' and on any machine when working on a given project. 
#' 
#' By default, \code{checkConfig()}
#' \itemize{
#' \item{reads the model release, the required base R version and possibly more objects 
#' as they are defined in the \code{releaseFile},}
#' \item{checks that all required folders of the project setup exist,}
#' \item{checks the base R version and installed packages. The default base R version currently 
#' is \code{3.4.4} and shall change to \code{3.6.3} soon.
#' The package’s versions should be in accordance with \code{zipLib}.}
#' \item{synchronizes the local package installation with \code{zipLib},}
#' \item{issues some messages to the console reporting the status (unless \code{silent=TRUE}).}
#' }
#' 
#' There are three mandatory Service Model R system variables:
#' \itemize{
#' \item{\code{project.home}: the root folder of your project,}
#' \item{\code{model.release}: the current model release (read from the \code{releaseFile} by default),}
#' \item{\code{data.dir}: the path to a folder where release-specific material is stored. 
#' (In the past, we used to store the model data in \code{data.dir}. 
#' Because that was inappropriate, large data sets are now saved on the \code{H:} drive.)}
#' }
#' 
#' The parameter \code{dontUpdate} accepts either a character vector or an integer value:
#' \itemize{
#' \item{If it is a character vector, the mentioned packages will not be updated,}
#' \item{if it is an integer value, installed packages that were built within the last \code{dontUpdate}
#' seconds will not be replaced by the versions in \code{zipLib}. 
#' This avoids that, during a production process, a package that has been just updated but not yet 
#' copied to \code{zipLib} is replaced by the older version in \code{zipLib} each time you run 
#' \code{checkConfig()}. This delay is 4 hours by default.}
#' }
#' 
#' @return 
#' \code{checkConfig()} returns one of three possible character strings:
#' \itemize{
#' \item{\code{"perfect"} if all checks succeeded,}
#' \item{\code{"ok"} if either the package installation has not been checked 
#' (e.g. if you set \code{checkInstallation=FALSE} or R failed to access the \code{W:} drive)
#' or some of the standard folders did not exist yet and had to be created, 
#' or you had to set \code{ignoreFreezing=TRUE} in order to access a closed model release,}
#' \item{\code{"failed"} if the setup could be prepared successfully.}
#' }
#' 
#' In the \code{"perfect"} case, a big \dQuote{OK} is issued to the console. 
#' 
#' @seealso \code{\link{smrSetup}}
#' 
#' @importFrom utils str capture.output
#' @importFrom smrPkgMgmt smrCheckPkgs smrUpdatePkgs
#' @export
checkConfig <- function(model.release = getRelease(releaseFile), 
                        project.home = getwd(),
                        data.dir = file.path(project.home, "4_Implementation", model.release), 
                        Rversion = requiredRversion(releaseFile = releaseFile, default = "3.4.4"), 
                        releaseFile = file.path(project.home, "4_Implementation/00_model.release.R"),
                        repos = "W:/0_Config/R_Release", 
                        zipLib = file.path(repos, Rversion, "libraryZip"),
                        Rlibrary = .libPaths()[1],
                        checkInstallation = all(dir.exists(zipLib)),
                        showInstallation = FALSE,
                        updatePackages = !showInstallation,
                        dontUpdate = 4*3600, # either names of packages or length of time interval in seconds
                        ignoreFreezing = FALSE, 
                        createAllDirs = TRUE,
                        silent = FALSE
                        ){

  message <- function(...){
    if (!silent) do.call(base::message, list(...), envir = globalenv())
    invisible()
  }
  
  # ==== Define project.home, releaseFile, data.dir ====
  project.home <- normalizePath(project.home, "/", mustWork = FALSE)

  releaseFile <- if (isAbsolutePath(releaseFile)){
    normalizePath(releaseFile, "/", mustWork = FALSE)
  } else {
    normalizePath(file.path(project.home, releaseFile), "/", mustWork = FALSE)
  }
  if (!file.exists(releaseFile)){
    stop("releaseFile does not exist: ", dQuote(releaseFile))
  }
  
  data.dir <- if (isAbsolutePath(data.dir)){
    normalizePath(data.dir, "/", mustWork = FALSE)
  } else {
    normalizePath(file.path(project.home, data.dir), "/", mustWork = FALSE)
  }  
  if (!dir.exists(data.dir)){
    stop("data.dir does not exist: ", dQuote(data.dir))
  }

  Rlibrary <- normalizePath(Rlibrary, "/", mustWork = FALSE)

  # ===== CHECK SETTINGS AND CHANGE THEM WHERE NECESSARY =====
  
  # ===== Initialization =====
  message("*** R-IAZI MODELLING ENVIRONMENT ***\n",
          "Project name:  ", basename(project.home), "\n", 
          "Release:       ", model.release, "\n",
          "R-version:     ", Rversion, "\n",
          "Release file:  ", releaseFile, "\n\n",
          "Standard (mandatory) objects in global environment:\n",
          "  project.home:  ", project.home, "\n",
          "  model.release: ", model.release, "\n",
          "  data.dir:      ", data.dir, "\n\n",
          "Checking project's and release's configuration...")
  perfect <- ok <- TRUE

  # Rlibrary directory exists
  check.dir.exists(Rlibrary)
  # Set .libPaths() if Rlibrary exists
  if (dir.exists(Rlibrary)) .libPaths(Rlibrary)

  # Check subdirectories
  expected.subdirs <- file.path(project.home,
                                if (createAllDirs){
                                  c("1_Management", "2_Specification", "3_Design", "4_Implementation", 
                                    "5_Release", "6_Test", "7_Documentation", "8_Delivery")
                                  } else "4_Implementation")
  subdirs.exist <- dir.exists(expected.subdirs)
  if (!all(subdirs.exist)){
    # Subdirectory 4 is strictly required: 
    if (!all(subdirs.exist[match(file.path(project.home, "4_Implementation"), expected.subdirs)])){ 
      warning("Mandatory subdirectory '4_Implementation' not found.",
              call. = FALSE)
      ok <- perfect <- FALSE
    } else {
      # Other subdirectories can be added 
      sapply(expected.subdirs[!subdirs.exist], dir.create)
      message("Missing subdirectories have been added: ", 
              paste(" ", expected.subdirs[!subdirs.exist], collapse = "\n"))
      perfect <- FALSE
    }
  }

  # ===== Check if the release has already been deployed =====
  freezeNotes <- file.path(project.home, "5_Release", model.release, "releaseIsFrozen")
  if (file.exists(freezeNotes)){
    warning("This release has already been released (=copied to W drive)\n",
            "   -> You may consider changing the release number...\n",
            "   [or re-run checkConfig() with ignoreFreezing=TRUE to proceed with this release number]")
    perfect <- FALSE
    if (!ignoreFreezing) ok <- FALSE
  }
  
  # ===== Data directory exists =====
  check.dir.exists(data.dir, spec = "Data directory", create = TRUE)

  # ===== check R installation =====
  if (!checkInstallation) message("NOTE: R packages not checked.")
  stp <- smrSetup(Rversion = Rversion, zipLib = zipLib, 
                  checkInstallation = checkInstallation,
                  showInstallation = showInstallation,
                  updatePackages = updatePackages,
                  dontUpdate = dontUpdate)
  ok <- ok && stp$ok
  perfect <- perfect && stp$perfect
  
  # ===== Output message ===== 
  if (perfect){
    message("Everything is fine!")
    if (interactive()){
      message("
   ****     **     **   **
  **  **    **    **    **
 **    **   **  **      **
**      **  ****        **
 **    **   **  **      **
  **  **    **    **
   ****     **      **  **")
    }
    }
  if (ok){
    # ==== Remove everything from the global env and assign mandatory objects ====
    rm(list = ls(all.names = TRUE, envir = globalenv()),       
       envir = globalenv())
    assign("project.home", project.home, envir = globalenv())
    assign("model.release", model.release, envir = globalenv()) 
    assign("data.dir", data.dir, envir = globalenv())
    
    # ==== Add "custom" objects read from '00_model.release.R' to global env ====
    readAdditionalObjects(releaseFile, silent = silent)

    message("R is ready to work on the project \"", basename(project.home), "\".\n")
    Sys.sleep(0.5)
  }
  
  # Return value
  if (perfect) "perfect" else if (ok) "ok" else "failed"
}

utils::globalVariables(c("Rversion", "model.release", "project.home"))

# ---------------------------------

# ==== Aux functions ====

# Function to read the release number from file
getRelease <- function(releaseFile){
  # read code from the file and identify line(s) defining model.release
  releaseFileCode <- parse(releaseFile)
  model.release.def <- sapply(releaseFileCode, 
         function(x){ 
           length(x) == 3 &&
           as.character(x[[1]]) == "<-" &&
           "model.release" %in% all.vars(x[[2]])})
  eval(releaseFileCode[seq_len(max(which(model.release.def)))])
  if (!exists("model.release", inherits = FALSE)){
    stop("Reading model.release from releaseFile ", 
         sQuote(releaseFile), " failed.")
  }
  model.release
}

# Is a path absolute (or relative)?
isAbsolutePath <- function(file){
  stopifnot(length(file) == 1)
  file <- gsub("\\/+", "/", file)
  identical(tolower(normalizePath(file, "/", mustWork = FALSE)),
            tolower(paste(strsplit(file, "[/|\\]")[[1]], collapse = "/")))
}

# read the R version from file
# if an object Rversion is defined in the file, then this is returned
# otherwise, default is returned
requiredRversion <- function(releaseFile = "4_Implementation/00_model.release.R", 
                             default = "3.4.4"){
  if (file.exists(releaseFile)){
    source(releaseFile, local = environment())
  }
  if (exists("Rversion", inherits = FALSE) &&
      grepl(.standard_regexps()$valid_R_system_version, Rversion))
    return(Rversion)
  default
}


# Add "custom" objects read from '00_model.release.R' to gloablenv
readAdditionalObjects <- function(releaseFile, silent = FALSE){
  code <- parse(releaseFile)
  
  eval(code, envir = (env <- new.env(parent = parent.frame())))
  additionalObjects <- setdiff(ls(env), c("model.release", "Rversion", "releaseFile"))
  # Put object names into the order they appear in the code
  additionalObjects <- intersect(all.vars(code), additionalObjects)
  # List additional objects in a message
  if (length(additionalObjects)){
    for (x in additionalObjects){
      assign(x, get(x, env, inherits = FALSE), envir = globalenv())
    }
    if (!silent){
      strConcise <- function(x) capture.output(
        str(x,  max.level = 0, vec.len = 1.5, nchar.max = 70, 
            digits.d = 3, give.attr = FALSE))
      message("\nSpecial objects read from release file added to global environment:\n",
              paste0("  ", format(paste0(additionalObjects, ": ")),
                     lapply(mget(additionalObjects, envir = env), strConcise),
              collapse = "\n"), "\n")
    }
  }
  invisible()
}


# ==== check.dir.exists: Function to check existence of a directory ====

check.dir.exists <- function(name, spec = "Directory", create = FALSE){
  ex <- dir.exists(name)
  for (i in seq_along(name)){
    if (!ex[i]){
      message(spec, " ", dQuote(name[i]), " does not exist.")
      assign("perfect", FALSE, parent.frame())
      if (create){
        message("Creating directory \"", name[i], "\"")
        dir.create(name[i])
      } else {
        assign("ok", FALSE, parent.frame())
      }
    }  
  }
  invisible(all(ex))
}  
