

# Write Excel Grid Vertical -----------------------------------------------


#' Write a List of Data Frame to a Vertically Stacked layout in xlsx file
#'
#' Write a list of data frames to each worksheets (tabs) in which each data frames are vertically stacked.
#' A wrapper around `writeData_gridVertical()`.
#'
#' @param ... One or more named list of data frames. Using syntax `tab_name = list_of_dfs`
#' @param file A file path to save the xlsx file
#' @param overwrite If 'TRUE' will save over 'file' if present
#' @param startCol (Numeric vector of length 1) specifying the starting column to write to.
#' @param startRow (Numeric vector of length 1) specifying the starting row of the first data frame to write to.
#' @param gapRow (Numeric vector of length 1) specifying number of empty rows (gaps) between each data frame
#' @param headerStyle Custom style to apply to column names. If `NULL`, generate style automatically.
#' Can input as `openxlsx::createStyle()` objects.
#' @param borders Either "none" (default), "surrounding", "columns", "rows" or respective abbreviations. If "surrounding", a border is drawn around the data. If "rows", a surrounding border is drawn with a border around each row. If "columns", a surrounding border is drawn with a border between each column. If "all" all cell borders are drawn.
#' @param writeData_args list of arguments pass to `openxlsx::writeData`
#'
#' @return as in `openxlsx::saveWorkbook`
#' @export
#'
#' @examples
#' if(FALSE){
#'  # not run
#'  write.xlsx_gV(
#'  tab1 = list(iris[1:2, ], mtcars),
#'  tab2 = list(Orange[1:3, ], mtcars),
#'  file = "path/to/excel-output.xlsx"
#'  )
#' }
write.xlsx_gV <- function(...,
                          file,
                          overwrite = FALSE,
                          startCol = 1,
                          startRow = 1,
                          gapRow = 1,
                          headerStyle = NULL,
                          borders = "columns",
                          writeData_args = list()
){

  ls_ls_df <- rlang::list2(...)
  sheet_names <- names(ls_ls_df)

  wb <- openxlsx::createWorkbook()

  # Iterate over Tabs (Worksheets)
  for (i in seq_along(ls_ls_df)) {

    # Take list of DF out
    ls_df <- ls_ls_df[[i]]

    # Header Style
    if (is.null(headerStyle)) {
      # Default HS
      hs <- createHeaderStyles(n = length(ls_df))
    }else{
      hs <- headerStyle
    }
    # Add Tabs
    openxlsx::addWorksheet(wb, sheetName = sheet_names[[i]], gridLines = TRUE)
    # Write Grid Data in each tabs
    rlang::exec(
      # FUN
      writeData_gridVertical,
      # Args
      wb,
      sheet = i, ls_df = ls_df,
      startRow = startRow, startCol = startCol,
      gapRow = gapRow,
      headerStyle = hs,
      borders = borders,
      !!!writeData_args
    )
  }

  ## Save Files
  openxlsx::saveWorkbook(wb, file = file, overwrite = overwrite)

}

# Write Data - Grid Vertical ----------------------------------------------



#' Write List of Data Frames to a Vertically Stacked Grid
#'
#' Write a list of Data Frames to worksheet with vertically stacked layout with optional styling.
#' A wrapper around `openxlsx::writeData()`
#'
#' @param wb A Workbook object containing a worksheet.
#' @param sheet The worksheet to write to. Can be the worksheet index or name.
#' @param ls_df A list of data frame
#' @param startCol (Numeric vector of length 1) specifying the starting column to write to.
#' @param startRow (Numeric vector of length 1) specifying the starting row of the first data frame to write to.
#' @param gapRow (Numeric vector of length 1) specifying number of empty rows (gaps) between each data frame
#' @param headerStyle Custom style to apply to column names. If `NULL`, generate style automatically.
#' Can input as `openxlsx::createStyle()` objects.
#' @param borders Either "none" (default), "surrounding", "columns", "rows" or respective abbreviations. If "surrounding", a border is drawn around the data. If "rows", a surrounding border is drawn with a border around each row. If "columns", a surrounding border is drawn with a border between each column. If "all" all cell borders are drawn.
#' @param ... passed to `openxlsx::writeData()`
#'
#' @return the same as `openxlsx::writeData()`
#' @export
#'
#' @examples NULL
writeData_gridVertical <- function(wb, sheet, ls_df,
                                   startCol = 1,
                                   startRow = 1,
                                   gapRow = 0,
                                   headerStyle = NULL,
                                   borders = "columns",
                                   ...
) {

  # Vector of Start Rows
  start_rows <- get_startRow_stack(ls_df, startRow =  startRow, gapRow = gapRow)
  ## Rep Args
  args_ls_long <- rep_args_len(..., length.out = length(ls_df))
  borders <- rep(borders, length.out = length(ls_df))

  for (i in seq_along(ls_df)) {
    # Header Styles
    ## Check if it is list of Style objects
    is_ls_hs <- is.list(headerStyle) && purrr::every(headerStyle, ~ inherits(.x, "Style"))

    if(is.null(headerStyle)){
      hs <- createHeaderStyles(n = length(ls_df))[[i]]
    } else if (is_ls_hs) {
      # If supply list of header style; supply them correspondingly
      if (length(headerStyle) != length(ls_df)) stop("Length of list of headerStyle must equals to data frames.")
      hs <- headerStyle[[i]]
    } else if (inherits(headerStyle, "Style")) {
      # If not, style the same
      hs <- headerStyle
    } else {
      stop("`headerStyle` must be a 'Style' object.")
    }
    # Dots
    args_ls <- lapply(args_ls_long, `[`, i)

    # Execute Write !!
    rlang::exec(openxlsx::writeData,
                # Args
                wb,
                sheet = sheet, x = ls_df[[i]],
                startRow = start_rows[[i]], startCol = startCol, headerStyle = hs,
                borders = borders[[i]],
                !!!args_ls
    )
  }

}
