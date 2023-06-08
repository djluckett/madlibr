#' Generic Function for Filling a Madlib
#'
#' This is the generic function for filling in the blanks in a madlib
#'   with the appropriate components.
#'
#' @param x An object with blanks to fill.
#'
#' @return A formatted string object.
#'
#' @export
fill = function(x, ...) {
  UseMethod("fill", x)
}

#' Fill Madlib
#'
#' Function to fill the blanks in a madlib with appropriately formatated
#'   components and return the result as a formatted string.
#'
#' @param x A madlib object
#' @param ... Various components, to be formatted and used to fill the
#'   blanks in x.
#'
#' @return A properly formatted string.
#'
#' @export
fill.madlib = function(x, ...) {
  # Get a list of all the madlib components
  components = list(...)

  # Check that the right number of components were provided
  num_blanks = count_blanks(x)
  if (num_blanks != length(components)) {
    stop("The wrong number of components was provided.")
  }

  # Format all components
  components = lapply(components, format_component)

  # Fill madlib using sprintf
  x = gsub("<__>", "%s", x)
  do.call(sprintf, c(x, components))
}

#' Count The Number of Blanks
#'
#' This is the generic function to count the number of
#'   blanks that an object has.
#'
#' @param x An object with blank spaces to fill, e.g., a madlib object.
#'
#' @return The number of blanks that are available to be filled.
#'
#' @export
count_blanks = function(x, ...) {
  UseMethod("count_blanks", x)
}

#' Count The Number of Blanks in a Madlib Object
#'
#' This function counts the number of blanks available to be filled
#'   in a madlib object.
#'
#' @param x A madlib object
#'
#' @return The number of blanks that are available to be filled in x.
#'
#' @export
count_blanks.madlib = function(x) {
  length(gregexpr("<__>", x)[[1]])
}
