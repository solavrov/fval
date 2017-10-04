
#' Common switch that takes expression as vector (helper.R)
#'
#' @param expression Expression vector
#' @param ... Cases and results
#'
#' @return Result of case that matches expression[i]
#' @export
#'
#' @examples
#' vectorSwitch(c("a","c"), a = 1, b = 2, c = 3, 4)
#'
vectorSwitch <- function(expression, ...) {

  result <- numeric()

  for (i in 1:length(expression)) {
    result[i] <- switch(expression[i], ...)
  }

  return (result)

}


#' Switch that takes expression and results as vectors (helper.R)
#'
#' @param expression Expression vector
#' @param ... Cases and vectors of results
#'
#' @return Result[i] of case that matches expression[i]
#' @export
#'
#' @examples
#' matrixSwitch(c("a","b","c"), a = 1:3, b = 11:13, c = 101:103, NA)
#'
matrixSwitch <- function(expression, ...) {

  result <- numeric()

  for (i in 1:length(expression)) {
    result[i] <- switch(expression[i], ...)[i]
  }

  return (result)

}






