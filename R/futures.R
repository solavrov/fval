
#' Return month number for a given month futures code
#'
#' @param code Month code (can be a vector)
#'
#' @return Month number
#' @export
getMonthNumberFromMonthCode <- function(code) {

  hlpr::vectorSwitch(code,
               F = 1, G = 2, H = 3, J = 4, K = 5, M = 6,
               N = 7, Q = 8, U = 9, V = 10, X = 11, Z = 12,
               NA)

}


#' Take expiration month number from futures ticker
#'
#' @param ticker Ticker (can be a vector)
#'
#' @return Expiration month number
#' @export
getMonthNumberFromFuturesTicker <- function(ticker) {

  code <- substr(ticker, nchar(ticker) - 1, nchar(ticker) - 1)
  month <- getMonthNumberFromMonthCode(code)

  return (month)

}


#' Take contract's year from futures ticker
#'
#' @param ticker Ticker (can be a vector)
#' @param decade Decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return Contract's year
#' @export
getYearFromFuturesTicker <- function(ticker, decade = "auto") {

  sysYear <- as.numeric(substr(Sys.Date(), 1, 4))

  presYear <- 10 * as.numeric(substr(Sys.Date(), 1, 3)) +
    as.numeric(substr(ticker, nchar(ticker), nchar(ticker)))

  autoYear <- numeric()

  for (i in 1:length(presYear)) {
    if (presYear[i] >= sysYear) {
      autoYear[i] <- presYear[i]
    } else {
      autoYear[i] <- presYear[i] + 10
    }
  }

  year <- switch(decade,
                 "auto" = autoYear,
                 "pres" = presYear,
                 "prev" = presYear - 10,
                 "next" = presYear + 10,
                 NA)

  return (year)

}


#' Take contract type code from futures ticker
#'
#' @param ticker Ticker (can be a vector)
#'
#' @return Contract type code
#' @export
getFuturesCodeFromTicker <- function(ticker) {

  substr(ticker, 1, nchar(ticker) - 2)

}

