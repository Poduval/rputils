
#' Price sensitivity analysis 
#' 
#' \code{psData()} creates an object suited to draw sensitivity plots.
#' It takes a \dQuote{muster} object and appraises it repeatedly with varying inputs of one variable.
#' 
#' @param model Character string: Which model to use (package name).
#' @param muster \code{data.frame} with 1 row: the data specifying the object.
#' @param varying Named list of length 1 with values of the \dQuote{varying} variable. 
#' Form: \code{list(variableName = valueList)}
#' @param fix Named list: optionally modify the value of other variable(s).
#' Form: \code{list(variableName = value)}
#' @param augment Logical: Execute \code{augment()} step? if \code{TRUE}, \code{ServiceR} is ran
#' with steps=2:4, otherwise 2:3.
#' Note that step 4 is required if you want to display the color codes.
#' (see argument \code{colorCodes} in \code{\link{plot.psData}}).
#' @param version Character string of length >=1: the model's release(s) to display.
#' @param prefix Prefix of environments in the model package's namespace.
#' @param \dots Passed to \code{ServiceR()}
#' 
#' @return 
#' Returns a list of \code{data.frame}s and additional information. 
#' 
#' The first element, \code{$data}, is a \code{data.frame} with the input data 
#' (muster with \code{varying} values in one variable);
#' the second element, \code{$appraisal}, is a list of one or several \code{data.frame}s 
#' (several in case of multiple releases or models) read from the \code{$result} 
#' component of the return value of \code{ServiceR}.
#' 
#' @seealso \link{psData_methods}, \link{psData_aux}
#' 
#' @examples
#' # After running section 1, you can jump directly to sections 2, 3, 4 or 5
#' 
#' # ==== 1.Define current and previous release of ORCHR1 and define example object ====
#' library(smr)
#' ORreleases <- smrReleases("ORCHR1")
#' model.release <- ORreleases[[length(ORreleases)]]
#' prev.release <-  ORreleases[[length(ORreleases)-1]]
#' musterObject <- smr(smrNamespace("ORCHR1")$testData$case01, 
#'                     model = "ORCHR1", steps = 1)[1, ]
#' musterObject
#' 
#' 
#' # ==== 2.Simple appplication of psData ====
#' # Create a psData object
#' psd <- psData("ORCHR1", version = model.release, 
#'               musterObject, varying = list(buildYear = 1990:2025))
#' str(psd, 2)
#' plot(psd)
#' # Draw points
#' plot(psd, geom = "geom_point", size = 3)
#' plot(psd) + geom_point(color = IAZIdarkgreen, size = 3)
#' # Add NAline
#' plot(psd) +  NAline(psd)
#' plot(psd, NAline = TRUE)  # easier
#' # colorCodes on x axis
#' plot(psd) + NAline(psd) + getColorCodes(psd)
#' plot(psd, NAline = TRUE, colorCodes = TRUE)  # easier
#' 
#' # fix -> changes the 'muster' object
#' plot(update(psd, fix = list(ortId = 2084)))  # (cheap location - y scale changes)
#' 
#' # discrete -> changes the default geom
#' plot(psd, discrete = TRUE, NAline = TRUE)
#' plot(psd, discrete = TRUE, NAline = TRUE) + 
#'   coord_cartesian(ylim = c(2000, 3000))
#' 
#' # discrete=TRUE and colorCodes=TRUE can not be combined:
#' \dontrun{plot(psd, discrete = TRUE, NAline = TRUE, colorCodes = TRUE)} # ERROR!
#' 
#' 
#' # ==== 3.psPlot: combines psData and plot in simple cases ====
#' psPlot("ORCHR1", version = model.release, musterObject, 
#'        varying = list(buildCondition = 0:5))
#' psPlot("ORCHR1", version = model.release, muster = musterObject, 
#'        varying = list(buildCondition = 0:5),
#'        NAline = TRUE, colorCodes = TRUE)
#' # Adjust text position of release names:         
#' psPlot("ORCHR1", version = model.release, muster = musterObject, textCorrection = -0.4,
#'        varying = list(buildCondition = 0:5),
#'        NAline = TRUE, colorCodes = TRUE)         
#' 
#' 
#' # ==== 4.Multiple curves ====
#' # --- Two versions of same model ---
#' psd2 <- psData("ORCHR1", version = c(model.release, prev.release), 
#'               musterObject, varying = list(buildYear = 1990:2025)) 
#' str(psd2, 2)
#' plot(psd2)
#' plot(psd2, NAline = TRUE)
#' plot(psd2, NAline = TRUE, colorCodes = TRUE)
#' 
#' # --- Use c.psData() to compare two locations ---
#' psd <- psData("ORCHR1", version = model.release, 
#'               musterObject, varying = list(buildYear = 1990:2025))
#' compare_BE_ZH <- c(
#'   "Bern 3011" = update(psd, fix = list(ortId = 1442)), 
#'   "Zurich 8001" = update(psd, fix = list(ortId = 3785)))
#' plot(compare_BE_ZH, colorCodes = TRUE)
#' 
#' # --- ORCHR1 versus PRCHR1 ---
#' varying <- list(surfaceContract = 30:200)
#' psOR <- psData("ORCHR1", musterObject, varying, version = "20.1.0")
#' psPR <- psData("PRCHR1", musterObject, varying, version = "20.1.0")
#' psBoth <- c(OR = psOR, PR = psPR)
#' plot(psBoth)
#' plot(psBoth, groupVarName = "offered or paid")  # groupVarName could also be set in c.psData()
#' plot(psBoth, NAline = TRUE, colorCodes = TRUE)
#' # Adjust text position :
#' plot(psBoth, NAline = TRUE, colorCodes = TRUE, textCorrection = 1)
#' 
#' 
#' # ==== 5.Advanced: several versions of different models  ====
#' # --- 2 versions of ORCHR1, 1 of PRCHR1 ---
#' varying <- list(surfaceContract = 30:200)
#' psOR <- psData("ORCHR1", musterObject, varying, version = c("20.1.0", "15.2.0"))
#' psPR <- psData("PRCHR1", musterObject, varying, version = c("20.1.0"))
#' plot(c(offered = psOR, paid = psPR))
#' plot(c(offered = psOR, paid = psPR), NAline = TRUE, colorCodes = TRUE)
#' plot(c(offered = psOR, paid = psPR), NAline = TRUE, colorCodes = TRUE, textCorrection = 2)
#' 
#' # --- 2 version of both ---
#' psOR2 <- psData("ORCHR1", musterObject, varying, version = c("20.1.0", "18.1.0"))
#' psPR2 <- psData("PRCHR1", musterObject, varying, version = c("20.1.0", "18.1.1"))
#' \dontrun{plot(c(OR = psOR2, PR = psPR2))} # ERROR: default color scale has only 3 levels!
#' plot(c(offered = psOR2, paid = psPR2), colorList = palette()[1:4])  # specify colors explicitly!
#' 
#' 
#' @importFrom utils getFromNamespace
#' @export
psData <- function(model, muster, varying, fix = NULL, augment = TRUE, 
                   version = NULL, prefix = "release", ...){
  # Part 1:
  # Prepare input data.frame according to input parameters muster, varying, fix
  varyVar <- names(varying)
  stopifnot(nrow(muster) == 1,
            is.list(varying), !is.null(varyVar))
  muster <- data.frame(muster)
  if (!is.null(fix)){
    fix <- as.data.frame(fix)
    stopifnot(nrow(fix) == 1)
    muster[names(fix)] <- fix
  }
  muster1 <- muster[rep(1, length(varying[[1]])), ]
  muster1[varyVar] <- varying[[1]]
  muster1$externalKey <- make.unique(muster1$externalKey)

  # Part 2:
  # Run ServiceR() to muster1 according to model, augment, prefix
  # -> returns appr, a list of data.frames (corresponding to the 'results' element of
  #    the output from ServiceR(), each having mandatory columns 'value' and 'varyVarColorCode'
  steps <- if (augment) 2:4 else 2:3
  calls <- appr <- vector("list", max(length(version), 1))
  if (is.null(version)){
    calls[[1]] <- match.call()
    appr[[1]] <- do.call(getFromNamespace("ServiceR", model), 
                         list(quote(muster1), steps = steps, ...))
  } else {
    cl <- match.call()
    for (i in seq_along(version)){
      cl$version <- version[[i]]
      calls[[i]] <- cl
      appr[[i]] <- do.call(getFromNamespace("ServiceR", model), 
                           list(quote(muster1), version = version[[i]], 
                                steps = steps, ...))
    }
  }
  names(appr) <- vapply(appr, function(x) names(attr(x, "w"))[[1]], FUN.VALUE = character(1))
  names(appr) <- sub(sprintf("^%s_", prefix), "", names(appr))
  # Extract result table + color code of varyVar
  appr[] <- lapply(appr, function(x){
    res <- x$result
    ptbl <- x$parameterInfo
    vvRows <- ptbl[, "name", drop = TRUE] == varyVar
    if (sum(vvRows) == nrow(res)){
      res$varyVarColorCode <- pmin(ptbl[vvRows, "qualityCode"], 2)
    }
    res
  })
  envir <- rep(list(parent.frame()), length(calls))
  names(calls) <- names(envir) <- names(appr)

  # Part 3:
  # Output the 'psData' object
  structure(list(data = muster1,
                 appraisal = appr,
                 varyVar = varyVar,
                 groupVarName = "release",
                 call = match.call(),
                 envir = envir,
                 calls = calls),
            class = "psData")
}



