
#' Return element "i" of "a" assuming that "a" has infinite tail filled with its last element
#'
#' @param a Any atom, vector, list
#' @param i Index of value
#'
#' @return Value with index i
#' @export
e <- function(a, i) {
  hlpr::getByIndex(a, i, atoms = c("FIBond", "TFutures", "EurOption"))
}


#' Check that all params have length = 1 or same length > 1
#'
#' @param ...
#'
#' @return Maximum length of params
#' @export
checkParams <- function(...) {
  hlpr::checkParams(..., atoms = c("FIBond", "TFutures", "EurOption"))
}


