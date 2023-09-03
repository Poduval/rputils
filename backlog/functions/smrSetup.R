# ===== Function to check R installation =====

#' Check the R installation for a smr project
#' 
#' @description
#' \code{smrSetup} is usually executed within \code{\link{checkConfig}()}, where it executes
#' the version checks of base R and add-on packages and updates the packages if required.
#'  
#' It can also be executed as an independent function (although this will hardly ever be necessary).
#' The package checks and updates are done with the functions
#' \code{\link{smrCheckPkgs}}, \code{\link{smrUpdatePkgs}} from the package \code{smrPkgMgmt}.
#' 
#' @inheritParams checkConfig
#' @param dontUpdate See the details section in \code{?\link{checkConfig}}.
#' 
#' @seealso \code{\link{checkConfig}}, \code{\link{smrCheckPkgs}}, \code{\link{smrUpdatePkgs}}.
#' 
#' @export
smrSetup <- function(Rversion = requiredRversion(releaseFile = "4_Implementation/00_model_release.R", 
                                                 default = "3.4.4"), 
                     zipLib = file.path("W:/0_Config/R_Release", Rversion, "libraryZip"),
                     checkInstallation = TRUE,
                     showInstallation = FALSE,
                     updatePackages = !showInstallation,
                     dontUpdate = 4 * 3600){
  ok <- perfect <- TRUE
  
  # --- Base R version ---
  # The R version currently running is the right one
  check.Rversion <- identical(Rversion, R.used <- with(R.version, paste0(major, ".", minor)))
  if (!check.Rversion){
    message("You are currently using R version ", R.used, ", but this project/release requires ", Rversion, "!")
    ok <- perfect <- FALSE
    }
  
  # --- Installed Packages ---
  if (checkInstallation && check.Rversion){
    invisible(lapply(zipLib, check.dir.exists))
    chkPkgs <- smrCheckPkgs(Rversion = Rversion, zipLib = zipLib)
    pkgs_ok <- is.na(chkPkgs$version_Zip) | with(attributes(chkPkgs), compV == "=" & compBD == "=")
    if (showInstallation){
      print(chkPkgs)
    }
    if (updatePackages){
      if (is.numeric(dontUpdate)){
        # select packages that have been built recently
        secs_since_build <- difftime(Sys.time(), chkPkgs$buildDate_Installed,
                                     units = "secs")
        notInstalled <- is.na(chkPkgs$version_Installed)
        secs_since_build <- as.numeric(secs_since_build)
        secs_since_build[notInstalled] <- Inf
        if (any(secs_since_build <= dontUpdate)){
          dontUpdate <- rownames(chkPkgs)[secs_since_build <= dontUpdate]
        }
        if (is.numeric(dontUpdate)) dontUpdate  <- character(0) 
      }
      updatePkgs <- smrUpdatePkgs(chkPkgs, dontUpdate = dontUpdate)
      if (length(attr(updatePkgs, "impossible"))) ok <- perfect <- FALSE
    } else if (!updatePackages && !all(pkgs_ok)){
      warning("NOTE: Packages are not updated (as updatePackages=FALSE)")
      perfect <- FALSE
    }
  }
  invisible(list(ok = ok, perfect = perfect))
}

