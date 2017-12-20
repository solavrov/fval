
#' Return month number for a given month code
#'
#' @param code Month code
#'
#' @return Month number
#' @export
month.Futures <- function(letter) {

  switch(
    letter,
    F = 1,
    G = 2,
    H = 3,
    J = 4,
    K = 5,
    M = 6,
    N = 7,
    Q = 8,
    U = 9,
    V = 10,
    X = 11,
    Z = 12,
    NA
  )

}


#' Return month code for a given month number
#'
#' @param month Month number
#'
#' @return Month code
#' @export
letter.Futures <- function(month) {

  switch(
    as.character(month),
    "1" = "F",
    "2" = "G",
    "3" = "H",
    "4" = "J",
    "5" = "K",
    "6" = "M",
    "7" = "N",
    "8" = "Q",
    "9" = "U",
    "10" = "V",
    "11" = "X",
    "12" = "Z",
    NA
  )

}


#' Is Futures ticker
#'
#' @param ticker Ticker
#'
#' @return TRUE if it is a ticker, FALSE otherwise
#' @export
isTicker.Futures <- function(ticker) {

  ticker <- toupper(ticker)

  year <- as.numeric(substr(ticker, nchar(ticker), nchar(ticker)))
  month <- month.Futures(substr(ticker, nchar(ticker) - 1, nchar(ticker) - 1))

  return (!is.na(year) && !is.na(month))

}


#' Take expiration month number from futures ticker
#'
#' @param ticker Ticker
#'
#' @return Expiration month number
#' @export
getMonth.Futures <- function(ticker) {

  ticker <- toupper(ticker)

  if (isTicker.Futures(ticker))
    month <- month.Futures(substr(ticker, nchar(ticker) - 1, nchar(ticker) - 1))
  else
    month <- NA

  return (month)

}


#' Take contract's year from futures ticker
#'
#' @param ticker Ticker
#' @param decade Decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return Contract's year
#' @export
getYear.Futures <- function(ticker, decade = "auto") {

  ticker <- toupper(ticker)

  if (isTicker.Futures(ticker)) {

    sysYear <- as.numeric(substr(Sys.Date(), 1, 4))

    presYear <- 10 * as.numeric(substr(Sys.Date(), 1, 3)) +
      as.numeric(substr(ticker, nchar(ticker), nchar(ticker)))

    if (presYear >= sysYear)
      autoYear <- presYear
    else
      autoYear <- presYear + 10

    year <- switch(
      decade,
      "auto" = autoYear,
      "pres" = presYear,
      "prev" = presYear - 10,
      "next" = presYear + 10,
      NA
    )

  } else {

    year <- NA

  }

  return (year)

}


#' Take contract type code from futures ticker
#'
#' @param ticker Ticker
#'
#' @return Contract type code
#' @export
getCode.Futures <- function(ticker) {

  ticker <- toupper(ticker)

  if (isTicker.Futures(ticker))
    code <- substr(ticker, 1, nchar(ticker) - 2)
  else
    code <- NA

  return (code)

}

