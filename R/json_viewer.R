#' Create json-viewer
#'
#' This is a wrapper around the \code{listviewer::\link[listviewer]{jsonedit}}
#' function, used to create an interactive (HTML) explorer for json-text.
#'
#' @param ...      arguments passed on to
#'   \code{listviewer::\link[listviewer]{jsonedit}}
#' @param height   character (or coercible to character),
#'   height of the widget in pixels or percent
#' @param width   character (or coercible to character),
#'   width of the widget in pixels or percent
#'
#' @seealso \code{listviewer::\link[listviewer]{jsonedit}}
#' @return HTML widget
#' @export
#'
json_viewer <- function(..., height = 250, width = "100%"){

  listviewer::jsonedit(..., height = height, width = width)
}
