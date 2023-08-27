
globalVariables(c("Parameter", "value", "modelRefDate", "loConf", "hiConf"  ))

#' Beta evolution functions
#' 
#' @description 
#' Functions to extract the beta values for all releases from a model package and return them in a data
#' table or plot the evolution in a line graph. 
#' 
#'  
#' @param model Model specification, as string constant or as expression
#' @param character.only A logical constant indicating whether model can be assumed to be a character string.
#' @param getSE A logical constant. If \code{TRUE} the StdError is extracted from the final model. 
#' @param colNames A character constant. If "dates" the model production dates are taken as data frame variable names.
#' @param x In \code{plotBetaEvolution} either a data frame as returned from getBetaEvolution or a model specification such as PPCH2A2,
#' In \code{plot} method for betaEvolution a data frame as returned from getBetaEvolution of class \code{betaEvolution}
#' @param ignore character vector listing the names of the variables which should be ignored
#' @param addZeroLine logical constant, if \code{TRUE} a horizonal line at y = 0 is added. 
#' @param ci logical constant, if \code{TRUE} then a confidence interval is drawn around the main curve. 
#' Note that for this the \code{betaTab} mast contain the standard errors as when produced by getBetaEvolution
#' with \code{getSE = TRUE}. 
#' @param ... arguments passed to other methods, currently not used. 
#' 
#' @name betaEvolution
#' 
#' 
#' @examples 
#' \dontrun{
#' (bb <- getBetaEvolution(ORCHR1))
#' plot(bb)  
#' plotBetaEvolution(ORCHR1)
#'  
#' plotBetaEvolution(PPCHA2, ci=TRUE)
#'
#' }
NULL



#' @describeIn betaEvolution  
#' Extracts a data frame of all betas from all releases present in package.
#' Returns a data frame with a variable Parameter which gives the parameter entries and one column per release. 
#' This function extracts the coefficients from a model package from 
#' release_xxxxxx$finalModel$coefficients    or   release_xxxxxx$fit$coefficients.
#' If getSE is TRUE then the standard error values are retrieved from 
#' release_xxxxxx$finalModel$StdError or  release_xxxxxxx$fit$StdError. If this variable doesn't exist a warning is issued. #' 
#' If getSE is TRUE and the standard error exists it is returned as attribute \code{se}
#' 
#' @importFrom smrUtils addclass int2date
#' @export
getBetaEvolution <- function(model, character.only = FALSE, getSE = FALSE, colNames = "release" ){
  
  # local function: converts a named numeric vector into a two column data frame:    
  makeDF <- function(x, n){  
    out <- data.frame(Parameter = names(x), x, stringsAsFactors = FALSE)
    names(out) <- c("Parameter", n)
    rownames(out) <- NULL
    out
    
  }  
  #  End local functions
  
  # Suppress "Registered S3 methods overwritten by" warnings:
  Sys.setenv("_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_" = 0)
  
  if (!(character.only)) 
    model <- deparse(substitute(model))
  if (length(model) > 1) stop("Cannot deal with more than one model at a time.")
  if (grepl("^\"", model) && grepl("\"$", model)) 
    model <- sub("^\"", "", sub("\"$", "", model))
  if (!(model %in% installed.packages()[,"Package"])) stop("Model package not installed!")
  
  #~~~~ get namespace of package: ~~~~
  # is model already in namesspace: 
  modelpreLoaded <- model %in% loadedNamespaces()
  model.ns       <- getNamespace(model)
  isenv <- eapply(model.ns, is.environment, all.names = TRUE)
  releaseNames <- names(isenv)[unlist(isenv, use.names = FALSE)]
  releaseNames <- sort(releaseNames[grepl("^release_",releaseNames) ] )
  releaseList  <- mget(releaseNames, model.ns)
  
  # two types of final models (PPCH and all others)
  finalModelPresent <- all(sapply(releaseList, function(x) "finalModel" %in% names(x)  ) )
  fitPresent        <- all(sapply(releaseList, function(x) "fit" %in% names(x)  ) )
  
  if(colNames == "dates"){
    releaseDates      <- sapply(releaseList, function(x) x$prodDate)
    releaseDates      <- makeDF(releaseDates, "releaseDates")
    releaseDates$releaseDates <- as.character(smrUtils::int2date(releaseDates$releaseDates))
    releaseDates$release <- substr(releaseDates$Parameter, 9,nchar(releaseNames[1]))
  } else { # if colNames is not "dates"
    releaseDates <- data.frame(Parameter = releaseNames, 
                               release   = substr(releaseNames, 9,nchar(releaseNames[1])), 
                               stringsAsFactors = FALSE
    )
    
  }
  
  if (finalModelPresent) {
    betaList     <- lapply(releaseList, function(x){ x$finalModel$coefficients })
    sePresent    <- all(sapply(releaseList, function(x) "StdError" %in% names(x$finalModel)))
    if(getSE) 
      if(sePresent) seList <- lapply(releaseList, function(x){ x$finalModel$StdError[,1] }) else seList <- NA
    
  } else if (fitPresent) {   # the PPCH case
    if ("summary" %in% names(releaseList[[1]]$fit)){ # case PPCH
      betaList     <- lapply(releaseList, function(x){ x$fit$summary[,"Beta"] })
      sePresent    <- all(sapply(releaseList, function(x) "StdError" %in% colnames(x$fit$summary)))
      if(getSE) 
        if(sePresent) seList <- lapply(releaseList, function(x){ x$fit$summary[,"StdError"] }) else seList <- NA
    } else if ("coefficients" %in% names(releaseList[[1]]$fit)) {# case PPCH2
      betaList     <- lapply(releaseList, function(x){ x$fit$coefficients })
      sePresent    <- all(sapply(releaseList, function(x) "StdError" %in% names(x$fit) ) )
      if(getSE) 
        if(sePresent) seList <- lapply(releaseList, function(x){ x$fit$StdError[,1] }) else seList <- NA  
    }
  }
  
  rm(model.ns)
  rm(releaseList)
  if (!modelpreLoaded) unloadNamespace(model)
  Sys.unsetenv("_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_")
  
  betaTab  <- makeDF(betaList[[1]], n = names(betaList)[1]) 
  for (i in 2:length(betaList)) {  
    betaTab <- merge(x = betaTab, y = makeDF(betaList[[i]], n = names(betaList)[i]), 
                     all = TRUE, sort = TRUE,
                     by = "Parameter")
  }
  
  if (colNames == "dates"){
    names(betaTab)[-1] <- releaseDates$releaseDates[ match(names(betaTab)[-1], releaseDates$Parameter)]
    
  } else { names(betaTab)[-1]  <- releaseDates$release[ match(names(betaTab)[-1], releaseDates$Parameter)] }
  
  
  if (getSE){
    if (!anyNA(seList)){
      seTab <- makeDF(seList[[1]], n = names(seList)[1]) 
      for (i in 2:length(seList)) {  
        seTab <- merge(x = seTab, y = makeDF(seList[[i]], n = names(seList)[i]), 
                       full = TRUE, 
                       by = "Parameter")
      }
      
      if (colNames == "dates"){
        names(seTab)[-1] <- releaseDates$releaseDates[ match(names(seTab)[-1], releaseDates$Parameter)]
        
      } else { names(seTab)[-1]  <- releaseDates$release[ match(names(seTab)[-1], releaseDates$Parameter)] }
      
      attr(betaTab, "se") <- seTab 
    } else {
      warning("No standard error to extract!")  
    }
  }  
  
  betaTab <- smrUtils::addclass(betaTab, "betaEvolution")
  
  betaTab
}

#' @describeIn betaEvolution
#' Plots beta evolutions from a model package. Returns a list of ggplot object each corresponding 
#' to a beta. 
#' @export
plotBetaEvolution <- function(x = "PPCH2A1", ignore = NULL, addZeroLine = FALSE, ci=FALSE){
  
  cl <-  match.call()
  cl[[1L]] <- quote(list)
  
  xString <- deparse(substitute(x))
  if (exists(xString, parent.frame())) {xClass  <- class(eval.parent(substitute(x)))
  } else xClass <- "name"
  
  if ( "name" %in% xClass) {
    
    cl$x     <- model <- deparse(substitute(x))
    myArgs   <- eval.parent(cl)
    #names(myArgs)[names(myArgs) == "x"] <- "betaTab"
    
    myArgs$x <- getBetaEvolution(model = model, character.only = TRUE, getSE = ci, colNames = "dates") 
    do.call("plot.betaEvolution", myArgs)
  } else if ("betaEvolution" %in% xClass ){ 
    myArgs   <- eval.parent(cl)
    #names(myArgs)[names(myArgs) == "x"] <- "betaTab"
    
    do.call("plot.betaEvolution", myArgs)
  } else if ("character" %in%  xClass ) {
    myArgs  <- eval.parent(cl)
    #names(myArgs)[names(myArgs) == "x"] <- "betaTab"
    
    betaTab <- getBetaEvolution(model = x, character.only = TRUE, getSE = ci, colNames = "dates") 
    myArgs$x <- betaTab
    do.call("plot.betaEvolution", myArgs) 
  }
  
  
}

#' @describeIn betaEvolution
#' plot method for betaEvolution objects as returned from getBetaEvolution
#' @importFrom utils capture.output installed.packages
#' @importFrom ggplot2 ggplot aes geom_line geom_point facet_wrap xlab ylab theme element_text labs geom_hline
#' @export
plot.betaEvolution <- function(x = NULL, ignore = NULL, addZeroLine = FALSE, ci=FALSE, ...){
  
  # if (class(model) != "character") model <- deparse(substitute(model))
  # 
  # suppressWarnings(
  #   x <- getBetaEvolution(model, character.only = TRUE, getSE = ci, colNames = "dates")
  # )
  
  sePresent <- FALSE
  if (ci & is.null(attr(x, "se"))){ warning("No standard errors to extract!")
  } else if (ci & !is.null(attr(x, "se"))) {sePresent <- TRUE}
  
  #betaLongTab  <- gather(x, key="modelRefDate", value = "value", -Parameter) 
  betaLongTab <- stats::reshape(x, varying = names(x)[-1], idvar="Parameter", v.names = "value", 
                                timevar = "modelRefDate", times = names(x)[-1], direction="long"
  )
  if (sePresent) {
    #seLongTab   <- tidyr::gather(attr(x, "se"), key="modelRefDate", value = "se", -Parameter)
    seLongTab  <- stats::reshape(attr(x, "se"), idvar = "Parameter", varying = names(attr(x, "se"))[-1], 
                                 v.names="se", timevar = "modelRefDate", 
                                 times = names(attr(x, "se"))[-1], direction="long"
    )
    # betaLongTab <- dplyr::left_join(betaLongTab, seLongTab, by = c("Parameter", "modelRefDate"))
    betaLongTab <- merge(betaLongTab, seLongTab, by = c("Parameter", "modelRefDate"), all.x=TRUE)
    
    rm(seLongTab)
  }
  
  qq <- stats::qnorm(0.975)
  if (sePresent){
    betaLongTab$loConf  <- betaLongTab$value -   qq*betaLongTab$se
    betaLongTab$hiConf  <- betaLongTab$value +   qq*betaLongTab$se}
  
  # remove rows where value is NA:
  #betaLongTab <- betaLongTab[!is.na(betaLongTab$value),]
  
  ggout <- list()
  for (i in x$Parameter){
    ggout[[i]] <- 
      ggplot(subset(betaLongTab, Parameter == i), 
             aes(x = factor(modelRefDate), y = value, group = 1)) + 
      geom_line(color  = IAZIgraph::IAZIdarkgreen, na.rm = TRUE) +
      geom_point(color = IAZIgraph::IAZIdarkgreen, na.rm = TRUE)  +
      labs(title = i, x="", y="Parameter value" )  + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
    if (sePresent) ggout[[i]] <- ggout[[i]] +
        geom_line(aes(y = loConf), size = 1, col = IAZIgraph::IAZIblue, linetype = "dashed") +
        geom_line(aes(y = hiConf), size = 1, col = IAZIgraph::IAZIblue, linetype = "dashed") 
    
    if(addZeroLine) {
      ggout[[i]] <- ggout[[i]] + geom_hline(yintercept = 0, col = IAZIgraph::IAZIdarkblue, lty = 2)
    }
    
  }
  
  ggout
  
}


