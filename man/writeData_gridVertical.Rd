% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xl-write.R
\name{writeData_gridVertical}
\alias{writeData_gridVertical}
\title{Write List of Data Frames to a Vertically Stacked Grid}
\usage{
writeData_gridVertical(
  wb,
  sheet,
  ls_df,
  startCol = 1,
  startRow = 1,
  gapRow = 1,
  headerStyle = NULL,
  borders = "columns",
  ...
)
}
\arguments{
\item{wb}{A Workbook object containing a worksheet.}

\item{sheet}{The worksheet to write to. Can be the worksheet index or name.}

\item{ls_df}{A list of data frame}

\item{startCol}{(Numeric vector of length 1) specifying the starting column to write to.}

\item{startRow}{(Numeric vector of length 1) specifying the starting row of the first data frame to write to.}

\item{gapRow}{(Numeric vector of length 1) specifying number of empty rows (gaps) between each data frame}

\item{headerStyle}{Custom style to apply to column names. If \code{NULL}, generate style automatically.
Can input as \code{openxlsx::createStyle()} objects.}

\item{borders}{Either "none" (default), "surrounding", "columns", "rows" or respective abbreviations. If "surrounding", a border is drawn around the data. If "rows", a surrounding border is drawn with a border around each row. If "columns", a surrounding border is drawn with a border between each column. If "all" all cell borders are drawn.}

\item{...}{passed to \code{openxlsx::writeData()}}
}
\value{
the same as \code{openxlsx::writeData()}
}
\description{
Write a list of Data Frames to worksheet with vertically stacked layout with optional styling.
A wrapper around \code{openxlsx::writeData()}
}
\examples{
NULL
}
