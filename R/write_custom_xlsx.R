#' Write Custom Excel
#'
#'
#' @description My custom wrapper around `openxlsx::write.xlsx()`.
#'
#'
#' @param x Object or a list of objects that can be handled by `openxlsx::writeData` to write to file
#' @param file file name in `.xlsx`
#' @param asTable Write using `openxlsx::writeDataTable()` as opposed to `openxlsx::writeData()`
#' @param borders Cell borders, Either "surrounding", "columns" or "rows" or NULL. If "surrounding", a border is drawn around the data. If "rows", a surrounding border is drawn a border around each row. If "columns", a surrounding border is drawn with a border between each column. If "all" all cell borders are drawn
#' @param keepNA  If `TRUE`, NA values are converted to #N/A (or na.string, if not `NULL`) in Excel, else `NA` cells will be empty. Defaults to `FALSE`.
#' @param colWidths_auto If `TRUE`, automatically adjust the column widths.
#' @param freeze_firstRow If `TRUE`, freezes the first row (equivalent to `firstActiveRow = 2`)
#' @param freeze_firstCol If `TRUE`, freezes the first column (equivalent to `firstActiveCol = 2`)
#' @param head_text Header text styling - one of "bold", "strikeout", "italic", "underline", "underline2" (passed to `openxlsx::createStyle`)
#' @param head_fgfill Header cell foreground fill colour. (passed to `openxlsx::createStyle`)
#' @param head_border Header Cell border. A vector of "top", "bottom", "left", "right" or a single string). (passed to `openxlsx::createStyle`)
#' @param head_halign Header Horizontal alignment of cell contents (passed to `openxlsx::createStyle`)
#' @param head_valign Header Vertical alignment of cell contents (passed to `openxlsx::createStyle`)
#' @param return_wb If `TRUE` Return workbook object.
#' @param ... to `openxlsx::write.xlsx()`
#'
#' @return Input Data `x` or workbook object
#' @export
#'
#' @examples
#' \dontrun{
#' write_custom_xlsx(list(a = iris, b = mtcars), "my_analysis.xlsx")
#' }
write_custom_xlsx <- function(x, file, asTable = FALSE,
                              borders = "columns", # Text border
                              keepNA = FALSE,
                              # Column width Auto
                              colWidths_auto = TRUE,
                              # freezePane
                              freeze_firstRow = FALSE,
                              freeze_firstCol = FALSE,
                              # Header style
                              head_text = "bold", # textDecoration
                              head_fgfill = "#d9ead3", # fgFill
                              head_border = "TopBottomLeftRight", # header border
                              head_halign = "center",
                              head_valign = "center",
                              # Return workbook obj or not
                              return_wb = TRUE,
                              ...){


  is.df <- is.data.frame(x)
  is.list_df <- all(purrr::map_lgl(x, is.data.frame))
  if(!is.df & !is.list_df) stop("`x` must be data frame or list of data frame", call. = F)

  sheets_seq <- if(is.df){ 1 }else{ seq_along(x) }

  ncols <- if(is.df){ ncol(x) }else{ purrr::map_int(x, ncol) }

  # Create Header Style
  head_style <- openxlsx::createStyle(textDecoration = head_text,
                                      halign = head_halign, valign = head_valign,
                                      fgFill = head_fgfill,
                                      border = head_border)

  wb <- openxlsx::write.xlsx(x, file, asTable = asTable,
                             headerStyle = head_style,
                             borders = borders,
                             keepNA = keepNA,
                             ...)

  if(colWidths_auto) {
    # Set Auto Column Width
    purrr::walk2(sheets_seq, ncols,
                 ~openxlsx::setColWidths(wb, sheet = .x, cols = seq(.y), widths = "auto"))
  }

  # Freeze First Row
  purrr::walk(sheets_seq ,
              ~openxlsx::freezePane(wb, sheet = .x ,
                                    firstRow = freeze_firstRow,
                                    firstCol = freeze_firstCol) )

  openxlsx::saveWorkbook(wb,  file, overwrite = T)

  if (return_wb) {
    invisible(wb)
  } else {
    invisible(x)
  }

}
