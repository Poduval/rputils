
# Function llm:
#   reduce an lm-object in size
#   returns a lighter object with classes "llm" and "lm"
# CAUTION: The result is not a valid lm-object, although "lm" will an element of class(.) - 
#          Most lm-methods will fail!
llm <- function(object){
  stopifnot(class(object) == "lm")
  vc <- vcov(object)
  sigma <- {
    w <- object$weights
    r <- object$residuals
    rss <- sum(if (is.null(w)) r^2 else r^2 * w)
    df <- object$df.residual
    sqrt(rss/df)
  } 
  object[c("residuals", "fitted.values", "model", "effects", "df.residual")] <- NULL
  object$qr$qr <- NULL
  object$sigma <- sigma
  object$vcov <- vc
  class(object) <- c("llm", "lm")
  object
}

# se.fit: Auxiliary function to calculate se.fit 
# (standard errors of predictions)
se.fit <- function(object, newdata){
  stopifnot(identical(class(object), c("llm", "lm")))
  na.action <- na.pass  
  # Build design matrix (lines taken from predict.lm):
  tt <- terms(object)
  Terms <- delete.response(tt)
  m <- model.frame(Terms, newdata, na.action = na.action, 
                   xlev = object$xlevels)
  if (!is.null(cl <- attr(Terms, "dataClasses"))) 
    .checkMFClasses(cl, m)
  X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
  
  # Calculation of se.fit without qr
  sqrt(rowSums(X * t(tcrossprod(object$vcov, X))))
}

# predict.llm:
# predict method for class llm
# Note that newdata must be specified (in contrast to predict.lm)
# The code is essentially taken from predict.lm - except the line containing the function se.fit
predict.llm <- function (object, newdata, se.fit = FALSE, ...){
  na.action <- na.pass

  tt <- terms(object)
  Terms <- delete.response(tt)
  m <- model.frame(Terms, newdata, na.action = na.action, 
                   xlev = object$xlevels)
  if (!is.null(cl <- attr(Terms, "dataClasses"))) 
    .checkMFClasses(cl, m)
  X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
  offset <- rep(0, nrow(X))
  if (!is.null(off.num <- attr(tt, "offset"))) 
    for (i in off.num) offset <- offset + eval(attr(tt, "variables")[[i + 1]], newdata, enclos = parent.frame())
  if (!is.null(object$call$offset)) 
    offset <- offset + eval(object$call$offset, newdata, enclos = parent.frame())

  p <- object$rank
  p1 <- seq_len(p)
  piv <- if (p){
    fromNsp <- getFromNamespace("qr.lm", "stats")
    fromNsp(object)$pivot[p1]
  }
  if (p < ncol(X) && !(missing(newdata) || is.null(newdata))) 
    warning("prediction from a rank-deficient fit may be misleading")

  beta <- object$coefficients
  predictor <- drop(X[, piv, drop = FALSE] %*% beta[piv])
  if (!is.null(offset)) 
    predictor <- predictor + offset
  
  predictor <- napredict(na.action, predictor)
  if (se.fit){
    se <- se.fit(object, newdata)
    se <- napredict(na.action, se)
  }  

  if (se.fit) 
    list(fit = predictor, se.fit = se, residual.scale = object$sigma)
  else predictor
}
