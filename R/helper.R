
#' Return element "i" of "a" assuming that "a" has infinite tail filled with its last element
#'
#' @param a Any atom, vector, list
#' @param i Index of value
#'
#' @return Value with index i
#' @export
E <- function(a, i) {
  hlpr::getByIndex(a, i, atoms = c("FIBond", "TFutures", "EurOption"))
}


#' Return attribute of object
#'
#' @param obj Object or list of objects
#' @param attrName Attribute name as character
#'
#' @return Attribute of object
#' @export
A <- function(obj, attrName) {
  hlpr::getAttribute(obj, attrName)
}


#' Check that all params have length = 1 or same length > 1
#'
#' @param ...
#'
#' @return Maximum length of params
#' @export
checkParams <- function(...) {
  hlpr::getLength(..., atoms = c("FIBond", "TFutures", "EurOption"))
}


