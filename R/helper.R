
#' Return element "i" of "a" assuming that "a" has infinite tail filled with its last element
#'
#' @param a Any atom, vector, list
#' @param i Index of value
#'
#' @return Value with index i
e <- function(a, i) {
  hlpr::getByIndex(a, i)
}

