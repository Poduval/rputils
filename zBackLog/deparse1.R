
# deparse1: deparse without line-breaking
deparse1 <- function(expr, ...)
  paste(sub("^ +", "", deparse(expr, ...)), collapse = "")

