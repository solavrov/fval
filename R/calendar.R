
#' List of RQuantLib day counters
#'
#' @export
dayCounter <- list(
  Actual360 = 0,
  Actual360FixEd = 1,
  ActualActual = 2,
  ActualBusiness252 = 3,
  OneDayCounter = 4,
  SimpleDayCounter = 5,
  Thirty360 = 6,
  Actual365NoLeap = 7,
  ActualActual.ISMA = 8,
  ActualActual.Bond = 9,
  ActualActual.ISDA = 10,
  ActualActual.Historical = 11,
  ActualActual.AFB = 12,
  ActualActual.Euro = 13
)


#' Return next business day using RQuantLib calendars
#'
#' @param date Date
#' @param calendar RQuantLib calendar name
#'
#' @return Next business day
#' @export
nextBizDay <- function(date = Sys.Date(), calendar = "WeekendsOnly") {

  for (i in 1:length(date)) {
    repeat {
      date[i] <- date[i] + 1
      if (RQuantLib::isBusinessDay(calendar, date[i])) break
    }
  }

  return (date)

}


#' Return previous business day using RQuantLib calendars
#'
#' @param date Date
#' @param calendar RQuantLib calendar name
#'
#' @return Next business day
#' @export
prevBizDay <- function(date = Sys.Date(), calendar = "WeekendsOnly") {

  for (i in 1:length(date)) {
    repeat {
      date[i] <- date[i] - 1
      if (RQuantLib::isBusinessDay(calendar, date[i])) break
    }
  }

  return (date)

}


#' Return number of next month
#'
#' @param month Number of present month
#'
#' @return Number of next month
#' @export
nextMonth <- function(month, year) {

  for (i in 1:length(month)) {
    month[i] <- month[i] + 1
    if (month[i] > 12) {
      month[i] <- 1
      year[i] <- year[i] + 1
    }
  }

  return (list(month = month, year = year))

}


#' Return date of last business day for a given month and year
#'
#' @param month Month number
#' @param year Year
#' @param calendar RQuantLib calendar name
#'
#' @return Date of last business day of a given month and year
#' @export
lastBizDay <- function(month, year, calendar = "WeekendsOnly") {

    prevBizDay(
      as.Date(
        paste0(nextMonth(month, year)$year,
               '-',
               nextMonth(month, year)$month,
               '-',
               '01')
        ),
      calendar)

}


#' Return date of first business day for a given month and year
#'
#' @param month Month number
#' @param year Year
#' @param calendar RQuantLib calendar name
#'
#' @return Date of first business day of a given month and year
#' @export
firstBizDay <- function(month, year, calendar = "WeekendsOnly") {

  nextBizDay(prevBizDay(
    as.Date(paste0(year, '-', month, '-', '01')),
    calendar)
  )

}

