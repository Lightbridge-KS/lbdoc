# Main: Read Markdown Content to Structured DF ---------------------------

#' Read Markdown File and Convert to a Structured DataFrame
#'
#' Reads a Markdown file using `readLines` and parses the content into a structured
#' DataFrame based on the Markdown headings. The function allows optional aggregation
#' of content under each heading and flexible reading of files with additional `readLines` arguments.
#'
#' @param file A character string naming a file, a connection, or literal data (either a
#'   single string or a raw vector) to be read.
#' @param summarize Logical, if `TRUE`, content under each heading is summarized into a single
#'   cell per heading level; if `FALSE`, each line retains its own row in the output DataFrame.
#' @param collapse The string to use when collapsing multiple lines of content under
#'   each heading when `summarize = TRUE`.
#' @param readLines_args A list of additional arguments to pass to `readLines()`. This can include
#'   any parameter that `readLines` accepts like `encoding`, etc.
#'
#' @return A `data.frame` where each row corresponds to a heading or content line. If
#'   `summarize` is `TRUE`, each heading level is condensed into a single entry. The columns
#'   of the DataFrame represent the hierarchical levels (headings) and the content under these headings.
#'
#' @examples
#' # Read a local Markdown file and convert to a DataFrame
#' # df <- read_md_to_df_str("path/to/your/file.md")
#'
#' # Read a Markdown file with content summarization disabled
#' # df <- read_md_to_df_str("path/to/your/file.md", summarize = FALSE)
#'
#' # Read a Markdown file with specific encoding
#' # df <- read_md_to_df_str("path/to/your/file.md", readLines_args = list(encoding = "UTF-8"))
#'
#' @export
read_md_to_df_str <- function(file,
                              summarize = TRUE,
                              collapse = ", ",
                              readLines_args = list()) {
  # Use do.call to pass additional arguments to readLines
  chr <- do.call(readLines, c(list(file), readLines_args, warn = FALSE))

  # Pass the character vector to parse_md_to_df_str()
  df <- parse_md_to_df_str(chr, summarize = summarize, collapse = collapse)
  df
}


# Main: Parse Markdown Content to Structured DF ---------------------------

#' Parse Markdown Content to a Structured DataFrame
#'
#' This function parses a vector of Markdown content into a structured DataFrame.
#' Each line of Markdown is analyzed for heading levels, and content under headings
#' is aggregated if specified. This allows for a tabular representation of hierarchical
#' document structures, useful for content analysis and manipulation.
#'
#' @param chr A character vector containing Markdown content, typically obtained
#'   from `readLines()`.
#' @param summarize Logical, if `TRUE`, the content under each heading is summarized
#'   into a single cell per heading level; if `FALSE`, each line retains its own row
#'   in the output DataFrame.
#' @param collapse The string to use when collapsing multiple lines of content under
#'   each heading when `summarize = TRUE`.
#'
#' @return A `data.frame` where each row corresponds to a heading or content line.
#'   If `summarize` is `TRUE`, each heading level is summarized into single entries
#'   per heading level. The DataFrame columns represent the hierarchical levels
#'   (headings) and the content under these headings.
#'
#'
#' @examples
#' markdown_text <- c("# Heading 1", "Content under heading 1", "## Subheading 1.1",
#' "Content under subheading 1.1", "# Heading 2", "Content under heading 2")
#' md_df <- parse_md_to_df_str(markdown_text)
#' print(md_df)
#'
#' @export
parse_md_to_df_str <- function(chr, summarize = TRUE, collapse = ", ") {
  h_lvs <- detect_h_levels_unique(chr)
  names(h_lvs) <- paste0("h", h_lvs)
  df_h <- purrr::map_dfc(h_lvs, ~rep_headings(chr, .x))
  df <- dplyr::bind_cols(line_num = seq_along(chr),
                         df_h,
                         content_body = extract_body_content_vec(chr))
  if(!summarize) return(df)
  df |>
    dplyr::group_by(!!!dplyr::syms(names(h_lvs))) |>
    dplyr::summarise(content_body = paste(na.omit(dplyr::na_if(content_body, "")),
                                          collapse  = collapse), .groups = "drop")

}

# Helper: Extract Body Content as Vector ----------------------------------


#' Extract Body Content as Vector
#'
#' This function takes a character vector containing Markdown lines and identifies
#' which lines are considered body content (i.e., not headings). Returns a character
#' vector of the same length where non-body lines are replaced with empty strings.
#'
#' @param chr A character vector of Markdown content.
#' @return A character vector with body content preserved and non-body content replaced with "".
#' @seealso \code{\link{parse_md_to_df_str}}
#' @noRd
extract_body_content_vec <- function(chr) {
  out <- character(length(chr))
  is_body <- detect_h_level_lines(chr) == 0
  out[is_body] <- chr[is_body]
  out[!is_body] <- ""
  out
}


# Helper: Replicate Headings ----------------------------------------------

#' Replicate Headings Across Their Scope
#'
#' This function replicates heading titles down their respective content scope until the next heading
#' of the same or higher level. Useful for attributing sections of text to specific headings in structured documents.
#'
#' @param chr Character vector of Markdown lines.
#' @param h Integer, the heading level to process.
#' @return A character vector where each element under a heading is filled with the heading's title.
#' @seealso \code{\link{parse_md_to_df_str}}
#' @noRd
rep_headings <- function(chr, h = 1) {
  df <- data.frame(
    line_num = seq_along(chr),
    h_lvs = detect_h_level_lines(chr),
    content = chr
  )
  h_content <- df$content[df$h_lvs == h]
  h_start_loc <- df$line_num[df$h_lvs == h]
  h_end_loc <- find_end_loc_by_heading(df$h_lvs, h)
  out <- rep_each_start_end(h_content, h_start_loc, h_end_loc, length(chr))
  out <- dplyr::na_if(trimws(stringr::str_remove(out, "^#+")), "")
  out
}



# Helper: Detect Heading Levels by Line -----------------------------------

#' Detect Heading Levels by Line
#'
#' Analyzes each line of a Markdown document and determines the heading level based on prefix hashes.
#' Lines not starting with '#' are considered body content and assigned a level of 0.
#'
#' @param chr Character vector of Markdown lines.
#' @return An integer vector indicating the heading level for each line.
#' @seealso \code{\link{parse_md_to_df_str}}
#' @noRd
detect_h_level_lines <- function(chr) {
  lv <- nchar(stringr::str_extract(chr, "^#+"))
  out <- ifelse(is.na(lv), 0, lv)
  out
}


# Helper: Detect Heading Levels -------------------------------------------

#' Detect Unique Heading Levels
#'
#' Extracts unique heading levels from a Markdown document to determine the hierarchical structure.
#' This is used to set up DataFrame column names corresponding to each unique heading level.
#'
#' @param chr Character vector of Markdown lines.
#' @return An integer vector of unique heading levels found in the document, sorted in ascending order.
#' @seealso \code{\link{parse_md_to_df_str}}
#' @noRd
detect_h_levels_unique <- function(chr){

  h_lines <- stringr::str_extract(chr, "^#+")
  h_lvs <- stringr::str_count(h_lines, "#")
  h_lvs_unique <- sort(unique(na.omit(h_lvs)))
  h_lvs_unique

}


# Helper: Replicate Vector with Starting and Ending location --------------

#' Replicate Vector Elements Between Specified Start and End Locations
#'
#' Given a vector and start/end positions, this function extends each vector element from its start to its end location,
#' filling the ranges accordingly. Useful for expanding headings or other markers over a range of data.
#'
#' @param x Vector of items to replicate.
#' @param start Integer vector indicating start positions.
#' @param end Integer vector indicating end positions.
#' @param length.out Optional integer, the desired length of the output vector.
#' @return A character vector with each element of 'x' replicated from 'start' to 'end'.
#' @seealso \code{\link{parse_md_to_df_str}}
#' @noRd
rep_each_start_end <- function(x, start, end, length.out=NULL) {

  out <- character(length = max(end))
  for (i in seq_along(x)) {
    content <- rep(x[i], end[i] - start[i] + 1)
    loc <- start[i]:end[i]
    out[loc] <- content
  }
  if(is.null(length.out)) return(out)
  ## Add NA to give final vector length as `length.out`
  out <- c(out, rep(NA, length.out - length(out)))
  out
}


# Helper: Find Ending Location by Headings --------------------------------

#' Find Ending Locations by Headings
#'
#' Determines the ending line number for each heading, which helps in defining the scope of content
#' under each heading. This function is used for proper replication and assignment of headings to sections.
#'
#' @param x Integer vector, usually the output of \code{\link{detect_h_level_lines}}, indicating heading levels per line.
#' @param h Integer, the specific heading level to process.
#' @return An integer vector with the ending line positions for each heading.
#' @seealso \code{\link{parse_md_to_df_str}}
#' @noRd
find_end_loc_by_heading <- function(x, h = 1) {
  h_consider_set <- c(which(x %in% seq_len(h)), length(x))
  h_loc <- c(which(x == h))

  res <- integer(length(h_loc))

  for (i in seq_along(h_loc)) {
    is_positive_diff <- (h_consider_set - h_loc[i]) > 0
    res[i] <- min(h_consider_set[is_positive_diff])
  }
  out <- ifelse(res == length(x), length(x), res - 1)
  out
}

