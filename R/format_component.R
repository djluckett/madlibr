#' Format Madlib Component
#'
#' This is the generic function to format a madlib component for
#'   filling a blank
#'
#' @param x A madlib component object.
#'
#' @return A string, to be used to fill a madlib blank.
#'
#' @export
format_component = function(x, ...) {
  UseMethod("format_component", x)
}
