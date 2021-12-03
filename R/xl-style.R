#' Create (Multiple) Header Styles
#'
#' A wrapper around `openxlsx::createStyle()`.
#' Each arguments of this functions automatically recycled.
#'
#' @param n Number of header styles to generate
#' @param fgFills foreground fills. If `NULL`, generate automatically.
#' @param halign 	Horizontal alignment of cell contents
#' @param valign A name Vertical alignment of cell contents
#' @param textDecoration Text styling.
#' @param border Cell border. A vector of "top", "bottom", "left", "right" or a single string).
#' @param ... args passed to `openxlsx::createStyle`
#'
#' @return A list of S4 `Style` object
#'
createHeaderStyles <- function(n = 1,
                               fgFills = NULL, # Default
                               halign = "center",
                               valign = "center",
                               textDecoration = "Bold",
                               border = "TopBottomLeftRight",
                               ...
){

  ## Default Foreground Fills
  if (is.null(fgFills)) {
    fgFills <- c("#cfe2f3", "#d9ead3", "#fce5cd", "#f4cccc", "#fff2cc", "#d0e0e3")
  }
  fgFills  <- rep(fgFills, length.out = n)
  halign <- rep(halign, length.out = n)
  valign <- rep(valign, length.out = n)
  textDecoration <- rep(textDecoration, length.out = n)
  border <- rep(border, length.out = n)
  # Dot Args
  args_ls_long <- rep_args_len(..., length.out = n)

  out <- vector("list", n)

  for (i in 1:n) {

    args_ls <- lapply(args_ls_long, `[`, i)

    out[[i]] <- rlang::exec(openxlsx::createStyle,
                            # Args
                            fgFill = fgFills[[i]],
                            halign = halign[[i]],
                            valign = valign[[i]],
                            textDecoration = textDecoration[[i]],
                            border = border[[i]],
                            !!!args_ls
    )
  }

  out

}
