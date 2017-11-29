
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


#' Return name of day counter
#'
#' @param counter Counter as a number
#'
#' @return Name
#' @export
counterName <- function(counter) {
  switch(as.character(counter),
         "0" = "Actual360",
         "1" = "Actual360FixEd",
         "2" = "ActualActual",
         "3" = "ActualBusiness252",
         "4" = "OneDayCounter",
         "5" = "SimpleDayCounter",
         "6" = "Thirty360",
         "7" = "Actual365NoLeap",
         "8" = "ActualActual.ISMA",
         "9" = "ActualActual.Bond",
         "10" = "ActualActual.ISDA",
         "11" = "ActualActual.Historical",
         "12" = "ActualActual.AFB",
         "13" = "ActualActual.Euro",
         NA)
}


#' Is given date a business day
#'
#' @param calendar Calendar name from RQuantLib
#' @param date Date
#'
#' @return TRUE if business day, FALSE otherwise
#' @export
isBizDay <- function(calendar, date) {
  RQuantLib::isBusinessDay(calendar, date)
}


#' Return number of days between two dates for a given day counter
#'
#' @param startDate Start date
#' @param endDate End date
#' @param dayCounter Day counter
#'
#' @return
#' @export
countDays <- function(startDate, endDate, dayCounter) {
  RQuantLib::dayCount(startDate, endDate, dayCounter)
}


#' Return date object
#'
#' @param date Date as a string
#' @param format dmy, mdy, ymd
#'
#' @return Date object
#' @export
parseDate <- function(date, format) {
  as.Date(lubridate::parse_date_time(as.character(date), format))
}


#' Return next business day using RQuantLib calendars
#'
#' @param date Date
#' @param calendar RQuantLib calendar name
#'
#' @return Next business day
#' @export
nextBizDay <- function(date = Sys.Date(), calendar = "WeekendsOnly") {

  repeat {
      date <- date + 1
      if (isBizDay(calendar, date)) break
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

  repeat {
      date <- date - 1
      if (isBizDay(calendar, date)) break
  }

  return (date)

}


#' Return number of next month
#'
#' @param month Number of present month
#' @param year Year
#'
#' @return List where $month is next month and $year is year of next month
#' @export
nextMonth <- function(month, year) {

  month <- month + 1
  if (month > 12) {
    month <- 1
    year <- year + 1
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

