#' Internal Constructor for Madlib Object
#'
#' This is the internal construction function for an object of class "madlib."
#'
#' @param x A character string representing a madlib object.
#'
#' @return An object of class "madlib."
new_madlib = function(x = character()) {
  stopifnot(is.character(x))
  structure(x, class = "madlib")
}

#' Validate Madlib Object
#'
#' This function checks whether a madlib object is valid.
#'
#' @param x An object of class "madlib."
#'
#' @return x. Primarily called for the side effect of throwing an error
#'   message if x is not a valid madlib.
validate_madlib = function(x) {
  # Verify that madlib text contains at least one blank ("<__>")

  # Check class and structure of object
  if (!("madlib" %in% class(x))) {
    stop("Attempting to validate an object that is not a madlib.")
  }
  if (!is.character(x)) {
    stop("A madlib must be a character string.")
  }

  # Verify that madlib text contains at least one blank to fill
  if (!grepl("<__>", x)) {
    stop("This madlib contains no blanks to fill.")
  }

  x
}

#' Madlib Style Text Formatting
#'
#' @param x A character string, representing a madlib object. Must have one
#'   or more blanks "<__>" to fill, using the \code{fill} method.
#'
#' @return An object of class "madlib."
#'
#' @export
madlib = function(x) {
  x = new_madlib(x)
  validate_madlib(x)

}
