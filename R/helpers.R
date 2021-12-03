


# Replicate Dot args ------------------------------------------------------



#' Replicate dynamic dots args to Specified Length
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments to replicate each by `length.out`
#' @param length.out amount of replication
#'
#' @return a list
#'
rep_args_len <- function(..., length.out = 1){

  # capture dot
  ls <- rlang::list2(...)
  if(length(ls) == 0L) return(NULL)

  out <- vector("list", length(ls))
  # Find where is NULL
  null_lgls <- sapply(ls, is.null)
  ## Assign NULL to where is NULL
  out[null_lgls] <- NULL
  ## Assign replicated values to the other location
  out[!null_lgls] <- lapply(ls[!null_lgls], rep, length.out = length.out)

  names(out) <- names(ls)
  out
}
