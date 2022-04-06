# function transformations:
# defines variable transformations to be applied in data frames
transformations <- function(...){
  out <- as.list(match.call())[-1]
  class(out) <- "transformations"
  nms <- names(out)
  # if (is.null(nms) || any(nms == ""))
  #   stop("Unnamed arguments not allowed in 'transformations'")
  used <- character(0)
  for (i in seq_along(out))
    used <- setdiff(c(used, all.vars(out[[i]])), 
                    nms[seq_len(i-1L)])
  attr(out, "used.vars") <- unique.default(used)
  out
}

# print method for class transformations
print.transformations <- function(x, ...){
  xdef <- paste(" ", format(names(x)), rep("=", length(x)), sapply(x, deparse1))
  cat("class 'transformations'\n")
  cat(xdef, sep = "\n")
  invisible(x)
}

# concatenate transformations objects and/or variable definitions
# arguments must be transformations objects (unnamed arguments) or new variable definitions (named arguments)
c.transformations <- function(...){
  dots <- list(...)
  if (length(dots) == 1) return(dots[[1]])
  if (!all(sapply(dots, inherits, "transformations")))
    stop("All arguments must inherit from class 'transformations'")
  out <- do.call(c, lapply(dots, unclass))
  used <- character(0)
  nms <- lapply(dots, names)
  for (i in seq_along(dots))
    used <- c(used, setdiff(attr(dots[[i]], "used.vars"),
                            unlist(nms[seq_len(i-1L)], recursive = FALSE, use.names = FALSE)))
  attr(out, "used.vars") <- unique(used)
  class(out) <- "transformations"
  out
}

# generic function 'add.transformations'
add.transformations <- function (.x, ...)
  UseMethod("add.transformations")

# default method: does nothing... (to prevent unintended use)
add.transformations.default <- function(.x, ...)
  stop("No appropriate 'add.transformations' method found for this object...")

# method for class "data.frame"
add.transformations.data.frame <- function(.x, ..., remove.vars = FALSE, 
                                           useColnames = TRUE, browseAt = NULL){
  ncol.x <- ncol(.x)
  transf <- c.transformations(...)
  if (length(transf) == 0){
    if (remove.vars) 
      .x[seq_len(ncol.x)] <- NULL
    return(.x)
  }
  nms <- names(transf)
  if (is.character(browseAt)) browseAt <- nms %in% browseAt
  if (is.logical(browseAt)) browseAt <- which(browseAt)
  for (i in seq_along(transf)){
    if (i %in% browseAt){
      message("browsing:\n",
              "  i = ", i, " (number of the transformation)\n", 
              "  transf[[i]] = ", deparse1(transf[[i]]))
      browser()
    }
    val <- eval(transf[[i]], .x, parent.frame(1))
    nm <- nms[[i]]
    val.is.matrix <- !is.null(ncol(val))
    colnms <- colnames(val)
    if (useColnames && !is.null(colnms)) nm <- NULL
    if (!useColnames && !is.null(nm)) colnms <- NULL
    if (is.null(colnms) && is.null(nm)) 
      stop("Could not determine column names for expression: ", deparse(transf[[i]])[[1]])
    if (val.is.matrix && ncol(val) == 1){
      val <- c(val)
      if (is.null(nm)) nm <- colnms
      val.is.matrix <- FALSE
    }
    if (val.is.matrix){
      if (is.null(colnms)) colnms <- paste0(nm, seq_len(NCOL(val)))
      .x[colnms] <- val
    } else {
      .x[[nm]] <- val
    }
  }
  if (remove.vars) 
    .x <- .x[nms]
  .x
}

  
# method for class "formula"
add.transformations.formula <- function(.x, ..., remove.vars = FALSE){
  stopifnot(inherits(.x, "formula"), length(.x) == 3)
  cl <- match.call()
  cl[[1]] <- as.name("c.transformations")
  cl$.x <- cl$remove.vars <- NULL
  transf <- eval(cl)
  if (length(transf) == 0) return(.x)
  stopifnot(inherits(transf, "transformations"))
  nms <- names(transf)
  uf <- remove.cond.vars(.x)
  uf <- update(uf, paste(". ~ . +", paste(nms, collapse = " + ")))
  if (isTRUE(all.equal(remove.vars, TRUE, check.attributes = FALSE)) ||
      (is.character(remove.vars) && length(remove.vars) > 0)){
    nms.rm <- unique(unlist(lapply(unname(transf), all.vars)))
    if (is.character(remove.vars)) nms.rm <- intersect(nms.rm, remove.vars)
    uf <- update(uf, paste(". ~ . -", paste(nms.rm, collapse = " - ")))
  }
  f.out <- uf  
  if (!is.null(cvars <- get.cond.vars(.x)))
    f.out <- paste(deparse1(uf), deparse1(cvars[[2]]), sep = " | ")
  as.formula(f.out, env = environment(.x))
}

# Aux functions
# function "get.cond.vars"
# extracts the 'conditioning' part on the rhs of a formula
get.cond.vars <- function(f){
  stopifnot(inherits(f, "formula"), length(f) == 3)
  splitf <- strsplit(deparse1(f), "\\|")[[1]]
  if (length(splitf) <= 1) return(NULL)
  out <- splitf[2]
  as.formula(paste("~ ", out), env = environment(f))
  }

# function "remove.cond.vars"
# removes the 'conditioning' part on the rhs of a formula
remove.cond.vars <- function(f){
  stopifnot(inherits(f, "formula"), length(f) == 3)
  out <- strsplit(deparse1(f), "\\|")[[c(1, 1)]]
  as.formula(out, env = environment(f))
}
