#' Internal Constructor for Madlib String Component
#'
#' This is the internal construction function for an
#'   object of class "madlib_string."
#'
#' @param x A character string, representing a standard string component
#'   to fill a madlib blank.
#'
#' @return An object of class "madlib_string."
new_madlib_string = function(x = character()) {
  stopifnot(is.character(x))
  structure(x, class = "madlib_string")
}

#' Validate Madlib String Component
#'
#' This function checks whether a madlib string component is valid.
#'
#' @param x An object of class "madlib_string."
#'
#' @return x. Primarily called for the side effect of throwing an error
#'   message if x is not a valid madlib_string.
validate_madlib_string = function(x) {
  # Check class and structure of object
  if (!("madlib_string" %in% class(x))) {
    stop("Attempting to validate an object that is not a madlib_string.")
  }
  if (!is.character(x)) {
    stop("A madlib_string must be a character string.")
  }

  x
}

#' String Component For Filling Madlib Blanks
#'
#' @param x A character string, to be used as a component to
#'   fill a madlib blank.
#'
#' @return An object of class "madlib_string."
#'
#' @export
madlib_string = function(x) {
  x = new_madlib_string(x)
  validate_madlib_string(x)
}

#' Format String Madlib Component
#'
#' This function returns a string component formatted for filling
#'   a madlib blank.
#'
#' @param madlib_string An object of class madlib_string.
#'
#' @return A character string.
#'
#' @export
format_component.madlib_string = function(madlib_string) {
  attributes(madlib_string) = NULL
  madlib_string
}
