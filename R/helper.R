
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


#' Normalize arguments converting elementary variables to vectors with rep(var, length)
#'
#' @param ... Elementary variables and vectors of the same length
#'
#' @return Nothing, the function alters arguments directly
#' @export
#'
#' @examples
#' x <- 1
#' y <- c("a", "b")
#' normalize(x, y)
normalize <- function(...) {

  params <- list(...)
  lens <- lengths(params)
  strings <- c(sapply(substitute(list(...)), deparse))[-1]
  env <- parent.env(environment())

  if (all(lens == 1 | lens == max(lens))) {
    for (i in 1:length(params))
      if (lens[i] == 1) assign(strings[i], rep(params[[i]], max(lens)), envir = env)
  } else {
    stop ("Vector length mismatch")
  }

}


