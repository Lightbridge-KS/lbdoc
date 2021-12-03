


# Start Rows Location to Write Stacked DFs --------------------------------


#' Get Start Rows to Write Stacked Data Frames
#'
#' Get vector of rows to write a stacked (top to bottom) data frame in a single worksheet
#'
#' @param ls_df A list of data.frame
#' @param startRow first row to write of a first data.frame
#' @param gapRow empty rows (gap) between each data.frame
#'
#' @return A named numeric vector indicating start rows of each data.frame
#'
get_startRow_stack <- function(ls_df, startRow = 1, gapRow = 0) {

  df_nms <- names(ls_df)
  ## Height of each DF
  ht_df <- sapply(ls_df, nrow) + 1

  out <- numeric(length(ls_df))
  for (i in seq_along(ls_df)) {
    ## First DF
    if(i == 1L){
      out[[1]] <- startRow
      next
    }
    ## Start Row of DFn
    ### Start point + sum of height of last dfs + number of gaps
    out[[i]] <-  startRow + sum(ht_df[1:(i - 1)]) + (i - 1)*gapRow
  }
  names(out) <- df_nms
  out
}
