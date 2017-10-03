
#' Constructor of european option object
#'
#' Attributes:
#' $type - call or put
#' $strike - strike
#' $time - time to expiration in years
#' $rate - risk free rate
#' $yield - dividend yield of underlying stock or else
#' $vol - volatility of underlying
#'
#' @return EurOption object
#' @export
EurOption <- function() {

  op <- list()
  class(op) <- "EurOption.fval"

  # default attributes
  op$type <- NA
  op$strike <- NA
  op$time <- NA
  op$rate <- NA
  op$yield <- NA
  op$vol <- NA

  return (op)

}


#' Get value of european option
#' Black-Scholes
#'
#' @param option option object
#' @param price price of underlying
#'
#' @return value of option
#' @export
getValueOfEurOption <- function(option, price) {

  d1 <-  1 / option$vol / sqrt(option$time) *
    (log(price / option$strike) +
       (option$rate - option$yield + option$vol^2/2) * option$time)

  d2 <- d1 - option$vol * sqrt(option$time)

  if (option$type == "call") {
    value <- price * exp(-option$yield * option$time) * pnorm(d1) -
      option$strike * exp(-option$rate * option$time) * pnorm(d2)
  } else if (option$type == "put") {
    value <- -price * exp(-option$yield * option$time) * pnorm(-d1) +
      option$strike * exp(-option$rate * option$time) * pnorm(-d2)
  } else {
    value <- NA
    cat("wrong type of option...\n")
  }

  return (value)

}


