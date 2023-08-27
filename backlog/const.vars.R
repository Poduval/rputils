const.vars <- function(data, groups = NULL){
  data <- as.data.frame(data)
  if (is.null(groups)) groups <- list(rep(1, nrow(data)))
  if (is.atomic(groups)) groups <- list(groups)
  uni <- aggregate(data, groups, function(x) length(unique(x)) == 1)
  uniqueInGroups <- sapply(uni[setdiff(seq_len(ncol(uni)), seq_along(groups))], all)
  names(which(uniqueInGroups))
  }
