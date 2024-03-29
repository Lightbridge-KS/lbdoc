% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xl-write.R
\name{write.xlsx_gV}
\alias{write.xlsx_gV}
\title{Write a List of Data Frame to a Vertically Stacked layout in xlsx file}
\usage{
write.xlsx_gV(
  ...,
  file,
  overwrite = FALSE,
  startCol = 1,
  startRow = 1,
  gapRow = 1,
  headerStyle = NULL,
  borders = "columns",
  freezePane_args = list(),
  writeData_args = list()
)
}
\arguments{
\item{...}{One or more named list of data frames. Using syntax \code{tab_name = list_of_dfs}}

\item{file}{A file path to save the xlsx file}

\item{overwrite}{If 'TRUE' will save over 'file' if present}

\item{startCol}{(Numeric vector of length 1) specifying the starting column to write to.}

\item{startRow}{(Numeric vector of length 1) specifying the starting row of the first data frame to write to.}

\item{gapRow}{(Numeric vector of length 1) specifying number of empty rows (gaps) between each data frame}

\item{headerStyle}{Custom style to apply to column names. If \code{NULL}, generate style automatically.
Can input as \code{openxlsx::createStyle()} objects.}

\item{borders}{Either "none" (default), "surrounding", "columns", "rows" or respective abbreviations. If "surrounding", a border is drawn around the data. If "rows", a surrounding border is drawn with a border around each row. If "columns", a surrounding border is drawn with a border between each column. If "all" all cell borders are drawn.}

\item{freezePane_args}{list of arguments pass to \code{openxlsx::freezePane()} (can be recycled to each tabs)}

\item{writeData_args}{list of arguments pass to \code{openxlsx::writeData()}}
}
\value{
Workbook object (invisible)
}
\description{
Write a list of data frames to each worksheets (tabs) in which each data frames are vertically stacked.
A wrapper around \code{writeData_gridVertical()}.
}
\examples{
if(FALSE){
 # not run
 write.xlsx_gV(
 tab1 = list(iris[1:2, ], mtcars),
 tab2 = list(Orange[1:3, ], mtcars),
 file = "path/to/excel-output.xlsx"
 )
}
}
