

# DOCX to Markdown --------------------------------------------------------

#' Convert Multiple DOCX Files to Markdown
#'
#' This function processes multiple DOCX files, converting each to a Markdown
#' format using Pandoc. It operates on vectors of input and output paths,
#' converting each file in sequence.
#'
#' @param input A character vector of DOCX file paths relative to `input_wd`.
#' @param input_wd A character string specifying the input working directory.
#'   Defaults to the current directory (`"."`).
#' @param output_path A character vector of output paths for the resulting Markdown files.
#' @param ... Additional arguments passed to `pandoc_convert_docx_md`.
#'
#' @return Invisible `NULL`. The function is used for its side effects (file creation).
#' @export
#' @examples
#' ## TODO
#' # pandoc_convert_multi_docx_md(c("doc1.docx", "doc2.docx"), output_path = c("out1.md", "out2.md"))
#' @importFrom rmarkdown pandoc_convert
#' @seealso \code{\link[pandoc_convert_docx_md]{pandoc_convert_docx_md}}
pandoc_convert_multi_docx_md <- function(input,
                                         input_wd = ".",
                                         output_path,
                                         ...
) {

  stopifnot(length(input) == length(output_path))

  for (i in seq_len(length(input))) {
    tryCatch( # Will continue even conversion error
      {
        pandoc_convert_docx_md(
          input = input[i],
          input_wd = input_wd,
          output_path = output_path[i]
        )

        cat("-", input[i], "->", basename(output_path[i]), "\n")
      },
      error = function(e) {
        message("ERROR: ", conditionMessage(e))
      }
    )
  }
}


#' Convert a DOCX File to Markdown
#'
#' Converts a single DOCX file to Markdown format using Pandoc. Optionally,
#' a YAML front matter with a title can be prepended to the output.
#'
#' @param input A character string specifying the DOCX file path relative to `input_wd`.
#' @param input_wd A character string specifying the input working directory.
#'   Defaults to the current directory (`"."`).
#' @param output_path A character string specifying the output path for the resulting Markdown file.
#' @param title Optionally, a character string specifying the title to be added to the Markdown file.
#'   If `NULL`, no title is added.
#' @param ... Additional arguments passed to Pandoc.
#'
#' @return Invisible `NULL`; the function writes the output directly to a file.
#' @export
#' @examples
#' # pandoc_convert_docx_md("example.docx", output_path = "example.md", title = "Example Document")
#' @seealso \code{\link[pandoc_convert_multi_docx_md]{pandoc_convert_multi_docx_md}}
pandoc_convert_docx_md <- function(input, # docx file relative to `input_wd`
                                   input_wd = ".",  # Input WD
                                   output_path, # Output md relative to project WD
                                   title = NULL,
                                   ...
) {
  temp_file <- tempfile()
  rmarkdown::pandoc_convert(
    input = input, from = "docx", to = "markdown",
    options = c("-o", temp_file),
    wd = input_wd,
    ...
  )

  # Read the existing contents of the file
  existing_content <- readLines(temp_file)
  # Open the file in write mode
  file_conn <- file(output_path, "w")

  if (!is.null(title)) {
    # Text to prepend
    pre_text <- glue::glue('---
title: "{title}"
format: html
---
')
    # Write the pre_text to the file
    cat(pre_text, "\n", file = file_conn)
  }

  writeLines(existing_content, file_conn)
  close(file_conn) # Close the file connection
}




# DOCX to Plain Text ------------------------------------------------------

#' Convert Multiple DOCX Files to Plain Text
#'
#' Converts several DOCX files to plain text, processing each file in the
#' provided vectors of input and output paths.
#'
#' @param input A character vector of DOCX file paths relative to `input_wd`.
#' @param input_wd A character string specifying the input working directory.
#'   Defaults to the current directory (`"."`).
#' @param output_path A character vector of output paths for the resulting text files.
#' @param ... Additional arguments passed to `pandoc_convert_docx_plain`.
#'
#' @return Invisible `NULL`; the function is used for its side effects (file creation).
#' @export
#' @examples
#' # pandoc_convert_multi_docx_plain(c("doc1.docx", "doc2.docx"), output_path = c("out1.txt", "out2.txt"))
#' @importFrom rmarkdown pandoc_convert
#' @seealso \code{\link[pandoc_convert_docx_plain]{pandoc_convert_docx_plain}}
pandoc_convert_multi_docx_plain <- function(input,
                                            input_wd = ".",
                                            output_path,
                                            ...) {

  stopifnot(length(input) == length(output_path))

  for (i in seq_len(length(input))) {
    tryCatch( # Will continue even conversion error
      {
        pandoc_convert_docx_plain(
          input = input[i],
          input_wd = input_wd,
          output_path = output_path[i]
        )

        cat("-", input[i], "->", basename(output_path[i]), "\n")
      },
      error = function(e) {
        message("ERROR: ", conditionMessage(e))
      }
    )
  }
}



#' Convert a DOCX File to Plain Text
#'
#' Converts a single DOCX file to plain text format using Pandoc.
#'
#' @param input A character string specifying the DOCX file path relative to `input_wd`.
#' @param input_wd A character string specifying the input working directory.
#'   Defaults to the current directory (`"."`).
#' @param output_path A character string specifying the output path for the resulting text file.
#' @param ... Additional arguments passed to Pandoc.
#'
#' @return Invisible `NULL`; the function writes the output directly to a file.
#' @export
#' @examples
#' # pandoc_convert_docx_plain("example.docx", output_path = "example.txt")
#' @seealso \code{\link[pandoc_convert_multi_docx_plain]{pandoc_convert_multi_docx_plain}}
pandoc_convert_docx_plain <- function(
    input, # docx file relative to `input_wd`
    input_wd = ".",  # Input WD
    output_path, # Output plain text relative to project WD
    ...
) {

  temp_file <- tempfile()
  rmarkdown::pandoc_convert(
    input = input, from = "docx", to = "plain",
    options = c("-o", temp_file),
    wd = input_wd,
    ...
  )
  # Read the existing contents of the file
  existing_content <- readLines(temp_file)
  # Open the file in write mode
  file_conn <- file(output_path, "w")
  writeLines(existing_content, file_conn)
  close(file_conn) # Close the file connection
}

