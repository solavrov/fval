
#' Common switch that takes vector as an expression argument - helper.R
#'
#' @param expression Expression
#' @param ... Cases
#'
#' @return Case that matches expression
#' @export
#'
#' @examples
#' vectorSwitch(c("a","c"), a = 1, b = 2, c = 3, 4)
vectorSwitch <- function(expression, ...) {

  result <- numeric()

  for (i in 1:length(expression)) {
    result[i] <- switch(expression[i], ...)
  }

  return (result)

}
