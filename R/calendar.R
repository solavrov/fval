
#' List of RQuantLib day counters
#'
#' @export
DAY_COUNTER <- list(
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
#' @param DAY_COUNTER Day counter
#'
#' @return Number of days between two dates for a given day counter
#' @export
countDays <- function(startDate, endDate, dayCounter) {
  RQuantLib::dayCount(startDate, endDate, dayCounter)
}


#' Return date object
#'
#' @param string Date as a string
#' @param format dmy, mdy, ymd
#'
#' @return Date object
#' @export
parseDate <- function(string, format) {
  if (all(is.na(string)))
    return (rep(NA, length(string)))
  else
    return (as.Date(lubridate::parse_date_time(as.character(string), format)))
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


#' Return next month's number and year
#'
#' @param month Present month's number
#' @param year Year
#' @param n Number of months to go forward
#'
#' @return List where $month is next month and $year is year of next month
#' @export
nextMonth <- function(month, year, n = 1) {

  year <- year + n %/% 12
  month <- month + n %% 12

  if (month > 12) {
    month <- month - 12
    year <- year + 1
  }

  return (list (month = month, year = year))

}


#' Return date of first day for a given month and year
#'
#' @param month Month number
#' @param year Year
#'
#' @return Date of first day for a given month and year
#' @export
firstDay <- function(month, year) {
  as.Date(paste0(year, '-', month, '-', '01'))
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
      firstDay(nextMonth(month, year)$month, nextMonth(month, year)$year),
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
  nextBizDay(prevBizDay(firstDay(month, year), calendar))
}



#' Disassemble date object to three numericals - day, month, year
#'
#' @param date Date object
#'
#' @return list of numericals $day, $month, $year
#' @export
disassembleDate <- function(date) {
  list(day = as.numeric(strftime(date, format = "%d")),
       month = as.numeric(strftime(date, format = "%m")),
       year = as.numeric(strftime(date, format = "%Y")))
}


#' Return date n month forward from given date
#'
#' @param date Date
#' @param n Number of months to go forward
#'
#' @return Date n month forward
#' @export
plusMonth <- function(date, n = 1) {

  d <- disassembleDate(date)
  m <- nextMonth(d$month, d$year, n)
  d$month <- m$month
  d$year <- m$year

  date <- as.Date(ISOdate(d$year, d$month, d$day))

  while (is.na(date)) {
    d$day <- d$day - 1
    date <- as.Date(ISOdate(d$year, d$month, d$day))
  }

  return (date)

}


#' Return vector of dates with given period from given start date
#'
#' @param startDate Start date
#' @param nper Number of periods
#' @param period Period - "month", "quarter", "half", "year"
#'
#' @return Vector of dates with given period from given start date
#' @export
getSchedule <- function(startDate, nper, period = "month") {

  monthSeq <-
    switch(period,
           month = seq(0, nper, 1),
           quarter = seq(0, nper * 3, 3),
           half = seq(0, nper * 6, 6),
           year = seq(0, nper * 12, 12),
           NA)

  schedule <- as.Date(mapply(plusMonth, startDate, monthSeq), origin = "1970-01-01")

  return (schedule)

}


#' Return span between two dates in whole weeks, months, quarters or years
#'
#' @param startDate Start date
#' @param endDate End date
#' @param roundBy Round by week, month, quarter, year
#'
#' @return Span between two dates
#' @export
roundSpan <- function(startDate, endDate, roundBy = "month") {

  nper <- ceiling(as.numeric(endDate - startDate) / 28)
  sch <- getSchedule(startDate, nper, roundBy)
  span <- length(which(sch <= endDate)) - 1

  return (span)

}



