# miscFunctions.R  


#' Counts how many observations are filtered out in each filtering step 
#' by filter conditions
#' @param data Data frame,  data set which is to be filtered
#' @param f character vector with filter conditions
#' @param type string, indicating format of output table, either "single", "cumulative", or "both"
#' @param digits integer, number of digits percentages should be rounded to. 
#' @details 
#' This function protocols the filtering in data preparation step 20. 
#' @examples 
#' \dontrun{
#' # Example from ORCHR1:
#' .filterText <-  XL.filter$functionR 
#' .filterCond <- as.language(paste0("(", XL.filter$functionR, ")", collapse = "&"))
#' countFilteredOut(rawdata, .filterText, type="both") 
#' raw1 <- droplevels(filter_(rawdata, .filterCond)) 
#' }
#' @importFrom plyr rename
#' @export
countFilteredOut <- function(data, f, type = "both", digits = 1){
  pastePerc <- function(x){
    x  <- as.character(x)
    ss <- character(length(x))
    ss[!is.na(x)] <- paste0(x[!is.na(x)], "%") 
    ss[is.na(x)]  <- NA
    ss
  }  
  
  
  ll  <- sapply(f, function(x){ res <- with(data, eval(parse(text = x) ) )
  table(factor(res, levels = c("TRUE", "FALSE")), useNA = "al") },
  simplify = FALSE
  )
  
  
  ##### prepare single table:
  if (type %in% c("single", "both")){
    out <- matrix(nrow = length(f)+1, ncol = 3)
    #colnames(out) <- c("FALSE", "TRUE", "NA")
    colnames(out) <- c("Filter_single", "Filter_perc", "NA_single")
    rownames(out) <- c("Number of objects before filtering",  names(ll) )
    out [, "Filter_single"] <- c(`Number of objects before filtering` = NA, sapply(ll, function(ff) ff["FALSE"] ))
    out [, "Filter_perc"]   <- c(`Number of objects before filtering` = NA, sapply(ll, function(ff) ff["FALSE"]/sum(ff)))
    out [, "NA_single"]     <- c(`Number of objects before filtering` = NA, sapply(ll, function(ff) ff[is.na(names(ff))] ))
    
    singleTab <- as.data.frame(out)
    singleTab$Filter_perc <- round(100*singleTab$Filter_perc, digits = digits) 
    singleTab$Filter_perc <- pastePerc(singleTab$Filter_perc )
  } # end if (type %in% c("single", "both"))
  
  
  ###### prepare cumulative table: 
  if (type %in% c("cumulative", "both")){
    d <- data
    cumulativeMatrix <- matrix(nrow = length(f) + 1, ncol = 5)
    rownames(cumulativeMatrix) <- c("Number of objects before filtering",  names(ll) )
    colnames(cumulativeMatrix) <- c("N.before", "Filter_cumulative", "Cum_perc", "NA_cum", "N.remaining")
    cumulativeMatrix[1, "N.remaining"] <- nrow(d)
    if (type %in% c("both", "cumulative")){
      for(i in seq_along(f)) {
        cumulativeMatrix[i+1, 1] <- nrow(d) 
        filter_res <- filter_resNAtoFALSE <- with(d, eval(parse(text=f[i])))
        filter_resNAtoFALSE[is.na(filter_resNAtoFALSE)] <- FALSE
        d <- d[filter_resNAtoFALSE, ]    # !! dplyr::filter_  discards rows where test results in NA !!
        cumulativeMatrix[i+1, "Filter_cumulative"]     <-  sum(filter_resNAtoFALSE== FALSE, na.rm = T)
        cumulativeMatrix[i+1, "NA_cum"]   <- sum(is.na(filter_res))
        cumulativeMatrix[i+1, "Cum_perc"] <-  sum(filter_resNAtoFALSE == FALSE, na.rm = T) / cumulativeMatrix[i+1, "N.before"]
        cumulativeMatrix[i+1, "N.remaining"] <- nrow(d)
      }
    }
    
    cumulativeTab <- as.data.frame(cumulativeMatrix)
    cumulativeTab$Cum_perc  <- round(100*cumulativeTab$Cum_perc , digits = digits) 
    
    lastRow <- data.frame(Filter_cumulative = sum(cumulativeTab$Filter_cumulative, na.rm=TRUE), 
                          NA_cum      =  sum(cumulativeTab$NA_cum, na.rm=TRUE),  
                          Cum_perc    = round(100*(sum(cumulativeTab$Filter_cumulative, na.rm=TRUE) / cumulativeMatrix[1, "N.remaining"]), digits=digits) , 
                          N.remaining = cumulativeTab$N.remaining[length(cumulativeTab$N.remaining)], 
                          row.names = "Total: ",
                          stringsAsFactors = FALSE
    )
    cumulativeTab[nrow(cumulativeTab)+1, names(lastRow)] <- lastRow
    cumulativeTab$Cum_perc  <- pastePerc(cumulativeTab$Cum_perc )
  } # end if (type %in% c("cumulative", "both"))
  
  
  if (type == "both")       {
    singleTab[nrow(singleTab)+1, c("Filter_single", "Filter_perc", "NA_single")] <- data.frame(NA, NA, NA, row.names ="Total: ",  stringsAsFactors = FALSE )
    outTab <- cbind(singleTab, cumulativeTab)
    
    # Format outTab: 
    
    outTab <- outTab[, setdiff(names(outTab), c(  "NA_single", "N.before"))]
    outTab <- plyr::rename(outTab, c(
      "Filter_single" = "Filter single", 
      "Filter_perc"   = "(%)", 
      "Filter_cumulative" = "Filter cumulative", 
      "NA_cum"            = "NA cumulative",
      "Cum_perc"      = "(%) ")
    )
    
    
  } else  if (type == "cumulative"){ 
    outTab <-  cumulativeTab
    outTab <- outTab[, setdiff(names(outTab), c("NA_cum", "N.before"))]
    outTab <- plyr::rename(outTab, c( 
      "Filter_cumulative" = "Filter cumulative", 
      "Cum_perc"      = "(%) ")
    )
    
  } else outTab <- singleTab
  
  outTab[is.na(outTab)] <- "-"
  outTab
} # end countFilteredOut  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 





#' Substitutes special German characters by English character combinations and removes other special characters
#' @details 
#' Removes trailing and leading spaces and substitutes special German characters with combinations of English characters. 
#' Finally \code{stringi::stri_trans_general }converts all non-ASCII-range punctuation, symbols, and Latin letters in an approximate ASCII-range equivalent.  
#' @param ss Character vector
#' @return Character vector of same length as ss but with special characters substituted and leading and trailing blanks removed. 
#' @examples 
#'  removeSpecials(c(" Über", "lkjö dü", "t-4", "k+b è"))
#'  #"Ueber"     "lkjoe_due" "t_4"       "k_b_e"  
#' @importFrom stringi stri_replace_all_fixed stri_trans_general
#' @export
removeSpecials <- function(ss) {
  
  out <- gsub("\\s+", "_", trimws(ss, "both"))  
  stringi::stri_replace_all_fixed(
    out, 
    # c("ä" , "ö",  "ü",  "Ä",  "Ö",  "Ü", "&", "-", "+", "/", "*", "#"), 
    # c("ae", "oe", "ue", "Ae", "Oe", "Ue", "", "_", "_", "_", "_", ""), 
    c("\u00e4" , "\u00f6",  "\u00fc",  "\u00c4",  "\u00d6",  "\u00dc", "&", "-", "+", "/", "*", "#"), 
    c("ae", "oe", "ue", "Ae", "Oe", "Ue", "", "_", "_", "_", "_", ""),
    
    vectorize_all = FALSE
  ) -> out
  stringi::stri_trans_general(out, "latin-ascii") 
} # end removeSpecials ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



