
#' Common switch that takes expression as vector (helper.R)
#'
#' @param expression Expression vector
#' @param ... Cases and results. Results must be same class
#'
#' @return Result of case that matches expression[i]
#' @export
#'
#' @examples
#' vectorSwitch(c("a","c"), a = 1, b = 2, c = 3, 4)
#'
vectorSwitch <- function(expression, ...) {

  result <- NA
  class(result) <- class(list(...)[[1]])

  for (i in 1:length(expression))
    result[i] <- switch(expression[i], ...)

  return (result)

}


#' Switch that takes expression and results as vectors (helper.R)
#'
#' @param expression Expression vector
#' @param ... Cases and vectors of results. Results must be same class
#'
#' @return Result[i] of case that matches expression[i]
#' @export
#'
#' @examples
#' matrixSwitch(c("a","b","c"), a = 1:3, b = 11:13, c = 101:103, NA)
#'
matrixSwitch <- function(expression, ...) {

  result <- NA
  class(result) <- class(list(...)[[1]])

  for (i in 1:length(expression))
    result[i] <- switch(expression[i], ...)[i]

  return (result)

}


#' Stretch elementary argument to reach length of others
#'
#' @param a Argument to stretch
#' @param ... Other arguments
#'
#' @return Stretched argument with rep(argument, length of others)
#' @export
#'
#' @examples
#' x <- 1
#' y <- c("a", "b")
#' z <- "c"
#' x <- stretch(x, y, z)
#' y <- stretch(y, x, z)
#' z <- stretch(z, y, x)
#'
stretch <- function(a, ...) {

  params <- append(list(...), a)

  lens <- lengths(params)

  if (all(lens == 1 | lens == max(lens))) {
    if (length(a) == 1) a <- rep(a, max(lens))
  } else {
    stop("Vector length mismatch")
  }

  return (a)

}


#' Return 1 if date within range and NA otherwise
#'
#' @param date Date (can be a vector)
#' @param earliestDate Earliest date, default value is minus infinity (-1e6)
#' @param latestDate Latest date, default value is plus infinity (1e6)
#'
#' @return 1 if date within range and NA otherwise
#' @export
checkDate <- function(date, earliestDate = -Inf, latestDate = Inf) {

  check <- 1 * (date >= earliestDate) * (date <= latestDate)
  check[check == 0] <- NA

  return (check)

}








