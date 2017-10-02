
#' Return list of RQuantLib day counters
#'
#' @return List of day counters
#' @export
dayCounterList <- function() {
  list(
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
    if (RQuantLib::isBusinessDay(calendar, date)) break
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
    if (RQuantLib::isBusinessDay(calendar, date)) break
  }

  return (date)

}


#' Return number of next month
#'
#' @param month Number of present month
#'
#' @return Number of next month
#' @export
nextMonth <- function(month) {

  month <- month + 1
  if (month > 12) month <- 1

  return (month)

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

  prevBizDay(as.Date(paste0(year, '-', nextMonth(month), '-', '01')), calendar)

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


#' Constructor of simple fixed income bond object
#'
#' Attributes:
#' $name - name as you wish
#' $isin - isin code
#' $currency - currency i.e. USD, RUB...
#' $faceAmount - face amount
#' $couponFreq - coupon frequency i.e. 2 for semiannual etc
#' $issueDate - issue date
#' $formula - yield formular where STD stands for standard convention, OFZ stands for OFZ convention
#' $dayCounter - day counter from dayCounterList
#' $cfactor - conversion factor for T-bonds-notes for T-futures
#' $couponDates - vector of coupon dates
#' $couponAmounts - vector of coupon amounts
#'
#' @param file Name of csv file that contains bond attributes. Heads should have names of attributes
#' @param dateFormat File date format from lubridate package i.e. "dmy", "mdy" etc
#' @param sep Separator i.e. comma, semicolon or else
#'
#' @return Bond object
#' @export
Bond <- function(file = NA,
                 dateFormat = "mdy",
                 sep = ",") {

  b <- list()
  class(b) <- "Bond.fval"

  #default attributes
  b$name <- NA
  b$isin <- NA
  b$currency <- NA
  b$faceAmount <- NA
  b$couponFreq <- NA
  b$issueDate <- NA
  b$formula <- "STD"
  b$dayCounter <- dayCounterList()$ActualActual
  b$cfactor <- NA
  b$couponDates <- NA
  b$couponAmounts <- NA


  if (!is.na(file)) {

    df <- read.csv(file, sep = sep)

    if (!is.null(df$name)) b$name <- as.character(df$name[1])
    if (!is.null(df$isin)) b$isin <- as.character(df$isin[1])
    if (!is.null(df$currency)) b$currency <- as.character(df$currency[1])
    if (!is.null(df$faceAmount)) b$faceAmount <- df$faceAmount[1]
    if (!is.null(df$couponFreq)) b$couponFreq <- df$couponFreq[1]

    if (!is.null(df$issueDate))
      b$issueDate <- as.Date(lubridate::parse_date_time(as.character(df$issueDate[1]), dateFormat))

    if (!is.null(df$formula)) b$formula <- as.character(df$formula[1])
    if (!is.null(df$dayCounter)) b$dayCounter <- df$dayCounter[1]
    if (!is.null(df$cfactor)) b$cfactor <- df$cfactor[1]

    if (!is.null(df$couponDates))
      b$couponDates <- as.Date(lubridate::parse_date_time(as.character(df$couponDates), dateFormat))

    if (!is.null(df$couponAmounts)) b$couponAmounts <- df$couponAmounts

  }

  return (b)

}


#' Print Bond object
#'
#' @param bond Bond object
#'
#' @return Info on screen
#' @export
print.Bond.fval <- function(bond) {
  cat("name:         ", bond$name, "\n")
  cat("isin:         ", bond$isin, "\n")
  cat("currency:     ", bond$currency, "\n")
  cat("faceAmount:   ", bond$faceAmount, "\n")
  cat("couponFreq:   ", bond$couponFreq, "\n")
  cat("issueDate:    ", as.character(bond$issueDate), "\n")
  cat("formula:      ", bond$formula, "\n")
  cat("dayCounter:   ", bond$dayCounter, "\n")
  cat("cfactor:      ", bond$cfactor, "\n\n")
  print(data.frame(
    couponDates = bond$couponDates,
    couponAmounts = bond$couponAmounts
  ))
}


#' Calculate coupon time i.e. days passed over days in coupon period for Bond object
#'
#' @param bond Bond object
#' @param settleDate Calculation date
#'
#' @return Coupon time for Bond object
#' @export
getCouponTime <- function(bond, settleDate = nextBizDay()) {

  nextPaymentIndex <- which(bond$couponDates > settleDate)[1]
  nextDay <- bond$couponDates[nextPaymentIndex]

  if (nextPaymentIndex >= 2) {
    prevDay <- bond$couponDates[nextPaymentIndex - 1]
  } else {
    prevDay <- bond$issueDate
  }

  period <- RQuantLib::dayCount(prevDay, nextDay, bond$dayCounter)
  daysPassed <- RQuantLib::dayCount(prevDay, settleDate, bond$dayCounter)

  time <- daysPassed / period

  return (time)

}


#' Calculate accrued interest for Bond object
#'
#' @param bond Bond object
#' @param settleDate Calculation date
#'
#' @return Accrued interest
#' @export
getAccrued <- function(bond, settleDate = nextBizDay()) {

  nextPaymentIndex <- which(bond$couponDates > settleDate)[1]
  coupon <- bond$couponAmounts[nextPaymentIndex]
  accrued <- coupon * getCouponTime(bond, settleDate)

  if (bond$formula == "OFZ") {
    accrued <- round(accrued / bond$faceAmount * 1000, digits = 2) *
      bond$faceAmount / 1000
  }

  return (accrued)

}


#' Calculate accrued interest for Bond object per 100 of face
#'
#' @param bond Bond object
#' @param settleDate Calculation date
#'
#' @return Accrued interest per 100 of face
#' @export
getAccruedPer100 <- function(bond, settleDate = nextBizDay()) {
  getAccrued(bond, settleDate) / bond$faceAmount * 100
}


#' Calculate value of Bond object
#'
#' @param bond Bond object
#' @param yield Bond yield
#' @param settleDate Calculation date
#'
#' @return Dirty value of bond object
#' @export
getValueOfBond <- function(bond, yield, settleDate = nextBizDay()) {

  spans <- as.numeric(bond$couponDates - settleDate)

  if (bond$formula == "OFZ") {

    factors <- (spans > 0) * 1 / (1 + yield) ^ (spans / 365)
    payments <- bond$couponAmounts

  } else {

    numOfFutureCoupons <- length(spans[spans > 0])

    factors <-
      1 / (1 + yield / bond$couponFreq) ^
      (1:numOfFutureCoupons - getCouponTime(bond, settleDate))

    payments <- tail(bond$couponAmounts, numOfFutureCoupons)

  }

  payments[length(payments)] <- payments[length(payments)] + bond$faceAmount

  value <- sum(payments * factors)

  return (value)

}


#' Calculate clean price of Bond object for 100 face
#'
#' @param bond Bond object
#' @param yield Bond yield
#' @param settleDate Calculation date
#'
#' @return Clean price of a Bond object
#' @export
getPriceOfBond <- function(bond, yield, settleDate = nextBizDay()) {

  price <-
    (getValueOfBond(bond, yield, settleDate) - getAccrued(bond, settleDate)) /
    bond$faceAmount * 100

  return (price)

}


#' Calculate yield of Bond object
#'
#' @param bond Bond object
#' @param price Bond clean price for 100 face
#' @param settleDate Calculation date
#' @param digits Accuracy as a number of digits after point
#'
#' @return Bond yield
#' @export
getYieldOfBond <- function(bond,
                           price,
                           settleDate = nextBizDay(),
                           digits = 4) {

  yieldRange <- c(-0.9, 0.9)
  f <- function(x) (getPriceOfBond(bond, x, settleDate) - price)
  solution <- uniroot(f, yieldRange, tol = 10 ^ (-digits - 1))
  yield <- round(solution$root, digits)

  return (yield)

}


#' Calculate PVBP of Bond object
#'
#' @param bond Bond object
#' @param price Clean price of Bond object for 100 face
#' @param settleDate Calculation date
#'
#' @return PVBP of Bond object
#' @export
getPVBPofBond <- function(bond, price, settleDate = nextBizDay()) {

  bp <- 10^-4
  yield <- getYieldOfBond(bond, price, settleDate)

  pvbp <- (getPriceOfBond(bond, yield + bp) -
             getPriceOfBond(bond, yield - bp)) / 2

  return (pvbp)

}



#' Calculate carry for Bond object for 100 face
#'
#' @param bond Bond object
#' @param price Clean price of Bond object for 100 face
#' @param settleDate1 Start date
#' @param settleDate2 End date
#' @param repoRate Funding term repo rate
#'
#' @return Carry for Bond object
#' @export
getCarryPer100 <- function(bond,
                           price,
                           settleDate1,
                           settleDate2,
                           repoRate) {

  inPlay <- which(bond$couponDates > settleDate1 & bond$couponDates <= settleDate2)

  couponAmounts <- bond$couponAmounts[inPlay] / bond$faceAmount * 100

  couponDates <- bond$couponDates[inPlay]

  carry <-
    getAccruedPer100(bond, settleDate2) -  getAccruedPer100(bond, settleDate1) +
    sum(couponAmounts * (1 + repoRate * as.numeric(settleDate2 - couponDates) / 360)) -
    (price + getAccruedPer100(bond, settleDate1)) *
    repoRate * as.numeric(settleDate2 - settleDate1) / 360

  return (carry)

}


#' Return list of TFutures actual data
#'
#' @return List of TFutures actual data
#' @export
futDataList <- function() {

  list(
    names = c(
      "2Y TNote Sep-17",
      "5Y TNote Sep-17",
      "10Y TNote Sep-17",
      "Ultra 10Y TNote Sep-17",
      "TBond Sep-17",
      "Ultra TBond Sep-17",
      "2Y TNote Mar-17",
      "2Y TNote Mar-18"
    ),

    tickers = c("TUU7", "FVU7", "TYU7", "UXYU7", "USU7", "WNU7", "TUH7", "TUH8"),

    deliveryDates = c(
      "2017-10-04",
      "2017-10-04",
      "2017-09-29",
      "2017-09-29",
      "2017-09-29",
      "2017-09-29",
      "2017-04-05",
      "2018-04-04"
    ),

    notionals = c(2*10^5, 10^5, 10^5, 10^5, 10^5, 10^5, 2*10^5, 2*10^5),

    ctdFiles = c(
      "fval_data/ctd_tuu7.csv",
      "fval_data/ctd_fvu7.csv",
      "fval_data/ctd_tyu7.csv",
      "fval_data/ctd_uxyu7.csv",
      "fval_data/ctd_usu7.csv",
      "fval_data/ctd_wnu7.csv",
      "fval_data/ctd_tuh7.csv",
      "fval_data/ctd_tuh8.csv"
    ),

    histFiles = c(
      "fval_data/hist_tuu7.csv",
      "fval_data/hist_fvu7.csv",
      "fval_data/hist_tyu7.csv",
      "fval_data/hist_uxyu7.csv",
      "fval_data/hist_usu7.csv",
      "fval_data/hist_wnu7.csv",
      "fval_data/hist_tuh7.csv",
      "fval_data/hist_tuh8.csv"
    )

  )

}


#' Return month number for a given month futures code
#'
#' @param code Month code
#'
#' @return Month number
#' @export
getMonthNumberFromMonthCode <- function(code) {

  switch(code,
         F = 1, G = 2, H = 3, J = 4, K = 5, M = 6,
         N = 7, Q = 8, U = 9, V = 10, X = 11, Z = 12,
         NA)

}


#' Take expiration month number from futures ticker
#'
#' @param ticker Ticker
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
#' @param ticker Ticker
#' @param decade Decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return Contract's year
#' @export
getYearFromFuturesTicker <- function(ticker, decade = "auto") {

  sysYear <- as.numeric(substr(Sys.Date(), 1, 4))

  presYear <- 10 * as.numeric(substr(Sys.Date(), 1, 3)) +
    as.numeric(substr(ticker, nchar(ticker), nchar(ticker)))

  if (presYear >= sysYear) {
    autoYear <- presYear
  } else {
    autoYear <- presYear + 10
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
#' @param ticker Ticker
#'
#' @return Contract type code
#' @export
getFuturesCodeFromTicker <- function(ticker) {

  substr(ticker, 1, nchar(ticker) - 2)

}


#' Return contract type by TFutures ticker
#'
#' @param ticker Ticker
#'
#' @return Contract type
#' @export
getContractType.TFutures <- function(ticker) {

  switch(getFuturesCodeFromTicker(ticker),
         TU = "2Y TNote",
         "3Y" = "3Y TNote",
         FV = "5Y TNote",
         TY = "10Y TNote",
         UXY = "Ultra 10Y TNote",
         US = "TBond",
         WN = "Ultra TBond",
         NA)

}


#' Return contract name by TFutures ticker
#'
#' @param ticker Ticker
#' @param decade decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return Contract name
#' @export
getName.TFutures <- function(ticker, decade = "auto") {

  name <- NA

  type <- getContractType.TFutures(ticker)

  if (!is.na(type)) {
    month <- month.abb[getMonthNumberFromFuturesTicker(ticker)][1]
    year <- getYearFromFuturesTicker(ticker, decade)
    name <- paste0(type, ' ', month, '-', year)
  }

  return (name)

}


#' Return notional amount by TFutures ticker
#'
#' @param ticker Ticker
#'
#' @return Notional amount
#' @export
getNotional.TFutures <- function(ticker) {

  switch(getFuturesCodeFromTicker(ticker),
         TU = 2e5,
         "3Y" = 1e5,
         FV = 1e5,
         TY = 1e5,
         UXY = 1e5,
         US = 1e5,
         WN = 1e5,
         NA)

}


#' Return model delivery date by TFutures ticker
#'
#' @param ticker Ticker
#' @param decade decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return Delivery date
#' @export
getDeliveryDate.TFutures <- function(ticker, decade = "auto") {


  futuresCode <- getFuturesCodeFromTicker(ticker)

  lastMonthBizDay <- lastBizDay(getMonthNumberFromFuturesTicker(ticker),
                                getYearFromFuturesTicker(ticker, decade),
                                calendar = "UnitedStates/GovernmentBond")

  thirdNextMonthBizDay <- lastMonthBizDay

  for (i in 1:3) thirdNextMonthBizDay <- nextBizDay(thirdNextMonthBizDay,
                                                    calendar = "UnitedStates/GovernmentBond")

  deliveryDate <- switch(
    futuresCode,
    TU = thirdNextMonthBizDay,
    "3Y" = thirdNextMonthBizDay,
    FV = thirdNextMonthBizDay,
    TY = lastMonthBizDay,
    UXY = lastMonthBizDay,
    US = lastMonthBizDay,
    WN = lastMonthBizDay,
    NA
  )

  return (deliveryDate)

}


#' Constructor of T-bond-note futures contract
#'
#' Attributes:
#' $name - name as you wish
#' $ticker - ticker like TYU7 etc
#' $notionalAmount - notional amount of futures
#' $deliveryDate - Last delivery date that is supposed to be an accual delivary date
#' $ctd - Ceapest-to-deliver Bond object
#'
#' @param ticker Bloomber ticker like TYU7 etc
#' @param ctdFileName name of ctd data file
#' @param dateFormat File date format from lubridate package i.e. "dmy", "mdy" etc
#'
#' @return TFutures object
#' @export
TFutures <- function(ticker = NA, ctdFileName = "", dateFormat = "mdy") {

  fut <- list()
  class(fut) <- "TFutures"

  #default attributes
  fut$name <- NA
  fut$ticker <- NA
  fut$notionalAmount <- NA
  fut$deliveryDate <- NA
  fut$ctd <- NA

  #attributes by ticker
  if (!is.na(ticker))  {

    if (!is.na(fut$name <- getName.TFutures(ticker))) {

      fut$ticker <- ticker
      fut$deliveryDate <- getDeliveryDate.TFutures(ticker)
      fut$notionalAmount <- getNotional.TFutures(ticker)

      if (ctdFileName == "") {
        ctdFileName <- paste0("fval_data/ctd_", tolower(ticker), ".csv")
        if (!file.exists(ctdFileName)) {
          ctdFileName <- ""
          cat("Can't find CTD's file", ctdFileName, "\nPlease, set up CTD manually...\n\n")
        }
      }

    } else {
      cat("Ticker is wrong. Default contract was created\n")
    }

  }

  if (ctdFileName != "") fut$ctd <- Bond(ctdFileName, dateFormat)

  return (fut)

}


#' Calculate model price of TFutures object for 100 notional
#'
#' @param fut TFutures object
#' @param ctdPrice CTD bond clean price
#' @param repoRate Term CTD repo rate
#' @param tradeDate Calculation date
#'
#' @return Model price of TFutures object
#' @export
getPriceOfTFutures <- function(fut, ctdPrice, repoRate, tradeDate = Sys.Date()) {

  t1 <- nextBizDay(tradeDate, calendar = "UnitedStates/GovernmentBond")
  carry <- getCarryPer100(fut$ctd, ctdPrice, t1, fut$deliveryDate, repoRate)
  price <- (ctdPrice - carry) / fut$ctd$cfactor

  return (price)

}


#' Calculate implied repo rate for TFutures object
#'
#' @param fut TFutures object
#' @param futPrice TFutures price for 100 notional
#' @param ctdPrice CTD bond clean price
#' @param tradeDate Calculation date
#'
#' @return Implied repo rate for TFutures object
#' @export
getImpliedRepoRate <- function(fut, futPrice, ctdPrice, tradeDate = Sys.Date()) {

  t1 <- nextBizDay(tradeDate, calendar = "UnitedStates/GovernmentBond")

  inPlay <- which(fut$ctd$couponDates > t1 & fut$ctd$couponDates <= fut$deliveryDate)

  couponAmounts <- fut$ctd$couponAmounts[inPlay] / fut$ctd$faceAmount * 100
  couponDates <- fut$ctd$couponDates[inPlay]

  irp <-
    (
      fut$ctd$cfactor * futPrice + getAccruedPer100(fut$ctd, fut$deliveryDate) + sum(couponAmounts) -
        ctdPrice - getAccruedPer100(fut$ctd, t1)
    ) /
    ((ctdPrice + getAccruedPer100(fut$ctd, t1)) * as.numeric(fut$deliveryDate - t1) / 360 -
       sum(
         couponAmounts * as.numeric(fut$deliveryDate - couponDates) / 360
       )
    )

  return (irp)

}


#' Calculate PVBP of TFutures object relative to CTD yield change
#'
#' @param fut TFutures object
#' @param ctdPrice CTD bond clean price for 100 face
#' @param repoRate CTD repo rate
#' @param tradeDate Calculation date
#'
#' @return PVBP of TFutures object relative to CTD yield change
#' @export
getPVBPCTDofTFutures <- function(fut, ctdPrice, repoRate, tradeDate = Sys.Date()) {

  bp <- 10^-4
  t1 <- nextBizDay(tradeDate, calendar = "UnitedStates/GovernmentBond")
  ctdYield <- getYieldOfBond(fut$ctd, ctdPrice, t1)

  bpPlusCTDprice <- getPriceOfBond(fut$ctd, ctdYield + bp, t1)
  bpMinusCTDprice <- getPriceOfBond(fut$ctd, ctdYield - bp, t1)

  pvbp <-
    (
      getPriceOfTFutures(fut, bpPlusCTDprice, repoRate, tradeDate) -
        getPriceOfTFutures(fut, bpMinusCTDprice, repoRate, tradeDate)
    ) / 2

  return (pvbp)

}


#' Calculate PVBP of TFutures object relative to CTD repo rate change
#'
#' @param fut TFutures object
#' @param ctdPrice CTD bond clean price for 100 face
#' @param repoRate CTD repo rate
#' @param tradeDate Calculation date
#'
#' @return PVBP of TFutures object relative to CTD repo rate change
#' @export
getPVBPRPofTFutures <- function(fut, ctdPrice, repoRate, tradeDate = Sys.Date()) {

  bp <- 10^-4

  pvbp <- (
    getPriceOfTFutures(fut, ctdPrice, repoRate + bp, tradeDate) -
      getPriceOfTFutures(fut, ctdPrice, repoRate - bp, tradeDate)
  ) / 2

  return (pvbp)

}


#' Return historical data analytics for TFuture pricing model
#'
#' @param fut TFutures object
#' @param histFile csv file that contains historical futures prices, CTD prices, repo rates
#' @param futField Head of futures prices column in csv file
#' @param ctdField Head of CTD prices column in csv file
#' @param repoField Head of CTD repo rates column in csv file
#' @param dateFormat File date format from lubridate package i.e. "dmy", "mdy" etc
#' @param sep Separator i.e. comma, semicolon or else
#'
#' @return Data frame with impliedRates, modelPrices, errors32nds
#' @export
getTFutModelAnalytics <- function(fut,
                                  histFile,
                                  futField = "futPrices",
                                  ctdField = "ctdPrices",
                                  repoField = "repoRates",
                                  dateFormat = "ymd",
                                  sep = ",") {

  hist <- read.csv(histFile, sep = sep)

  hist$dates <- as.Date(lubridate::parse_date_time(as.character(hist$dates), dateFormat))

  impliedRates <- hlpr::KVVV(getImpliedRepoRate,
                             fut,
                             hist[[futField]],
                             hist[[ctdField]],
                             hist$dates)

  modelPrices <-
    hlpr::KVVV(getPriceOfTFutures, fut, hist[[ctdField]], hist[[repoField]], hist$dates)

  errors32nds <- (modelPrices - hist[[futField]]) * 32

  analytics <- data.frame(
    dates = hist$dates,
    futPrices = hist[[futField]],
    repoRates = hist[[repoField]],
    ctdPrices = hist[[ctdField]],
    impliedRates,
    modelPrices,
    errors32nds
  )

  return (analytics)

}


#' Chart historical TFutures pricing model errors for XXU7 contracts
#'
#' @param ticker Ticker or its number from futDataList like TYU7 etc
#' @param ois3 Chart errors for USD OIS 3M as a term repo rate if TRUE
#' @param ois4 Chart errors for USD OIS 4M as a term repo rate if TRUE
#'
#' @return Chart of TFutures pricing model errors
#' @export
demoTFutModelError <- function(ticker, ois3 = FALSE, ois4 = FALSE) {

  cat("ATTENTION: Need demo data files in fval_data folder\n")

  fut <- TFutures(ticker)
  i <- which(futDataList()$tickers == fut$ticker)

  cat("Charting", fut$name, fut$ticker, "...\n")

  analytBloom <- getTFutModelAnalytics(fut, futDataList()$histFiles[i])
  analytOis3 <- getTFutModelAnalytics(fut, futDataList()$histFiles[i], repoField = "ois3")
  analytOis4 <- getTFutModelAnalytics(fut, futDataList()$histFiles[i], repoField = "ois4")

  yRange <-
    c(
      min(
        analytBloom$errors32nds,
        analytOis3$errors32nds,
        analytOis4$errors32nds
      ),
      max(
        analytBloom$errors32nds,
        analytOis3$errors32nds,
        analytOis4$errors32nds
      )
    )

  plot(
    x = analytBloom$dates,
    y = analytBloom$errors32nds,
    xlab = "Date",
    ylab = "Error (32nds)",
    main = paste0("Model error, ", fut$name, ", ", fut$ticker),
    type = "l",
    lwd = 3,
    col = "blue",
    ylim = yRange
  )


  chartLegend <- c("Bloomberg Rate")
  chartLty <- c(1)
  chartLwd <- c(3)
  chartCol <- c("blue")

  if (ois3) {
    lines(
      x = analytOis3$dates,
      y = analytOis3$errors32nds,
      type = "l",
      lwd = 3,
      col = "green"
    )

    chartLegend <- append(chartLegend, "USD OIS 3M")
    chartLty <- append(chartLty, 1)
    chartLwd <- append(chartLwd, 3)
    chartCol <- append(chartCol, "green")

  }

  if (ois4) {
    lines(
      x = analytOis4$dates,
      y = analytOis4$errors32nds,
      type = "l",
      lwd = 3,
      col = "red"
    )

    chartLegend <- append(chartLegend, "USD OIS 4M")
    chartLty <- append(chartLty, 1)
    chartLwd <- append(chartLwd, 3)
    chartCol <- append(chartCol, "red")

  }

  legend(
    "bottom",
    legend = chartLegend,
    lty = chartLty,
    lwd = chartLwd,
    col = chartCol,
    bg = "transparent",
    bty = "n",
    horiz = TRUE
  )

  cat("DONE!!!")

}


#' Chart ageing of PVBP relative to term repo rate for XXU7 contracts
#'
#' @param ticker Ticker or its number from futDataList like TYU7 etc
#'
#' @return Chart of PVBP relative to term repo rate over time
#' @export
demoTFutSensToRP <- function(ticker) {

  cat("ATTENTION: Need demo data files in fval_data folder\n")

  fut <- TFutures(ticker)
  i <- which(futDataList()$tickers == fut$ticker)
  hist <- read.csv(futDataList()$histFiles[i])
  hist$dates <- as.Date(lubridate::parse_date_time(as.character(hist$dates), "ymd"))

  pvbprp <- hlpr::KKKV(getPVBPRPofTFutures,
                       fut,
                       hist$ctdPrices[1],
                       hist$repoRates[1],
                       as.Date(hist$dates)) * 32

  plot(
    as.Date(hist$dates),
    pvbprp,
    type = "s",
    col = "blue",
    lwd = 3,
    xlab = "Date",
    ylab = "PVBP (32nds)",
    main = paste0("Ageing of Repo Rate PVBP, ",
                  fut$name,
                  ", ",
                  fut$ticker)
  )

  cat("DONE!!!")

}


#' Chart historical interest rates for some period of 2017 related to XXU7 contracts
#'
#' @param histFile History file where function takes interest rate data
#' @param ois3 Chart USD OIS 3M if TRUE
#' @param ois4 Chart USD OIS 4M if TRUE
#'
#' @return Chart of historical interest rates
#' @export
demoTFutRatesHistory <- function(histFile = "fval_data/hist_tuu7.csv",
                                 ois3 = FALSE,
                                 ois4 = FALSE) {

  cat("ATTENTION: Need demo data files in fval_data folder\n")

  hist <- read.csv(histFile)
  hist$dates <- as.Date(lubridate::parse_date_time(as.character(hist$dates), "ymd"))

  yRange <-
    c(
      min(
        hist$fedLow,
        hist$fedUp,
        hist$repoRates,
        hist$ois3,
        hist$ois4
      ),
      max(
        hist$fedLow,
        hist$fedUp,
        hist$repoRates,
        hist$ois3,
        hist$ois4
      )
    )

  chartLegend <- c("Fed Target", "Bloomberg Rate")
  chartLty <- c(1, 1)
  chartLwd <- c(3, 3)
  chartCol <- c("black", "blue")

  plot(
    x = as.Date(hist$dates),
    y = hist$fedLow,
    type = "l" ,
    lwd = 3,
    col = "black",
    xlab = "Date",
    ylab = "Rate",
    main = "Interest Rates",
    ylim = yRange
  )

  lines(
    x = as.Date(hist$dates),
    y = hist$fedUp,
    type = "l",
    lwd = 3,
    col = "black"
  )

  lines(
    x = as.Date(hist$dates),
    y = hist$repoRates,
    type = "l",
    lwd = 3,
    col = "blue"
  )


  if (ois3) {
    lines(
      x = as.Date(hist$dates),
      y = hist$ois3,
      type = "l",
      lwd = 3,
      col = "green"
    )

    chartLegend <- append(chartLegend, "USD OIS 3M")
    chartLty <- append(chartLty, 1)
    chartLwd <- append(chartLwd, 3)
    chartCol <- append(chartCol, "green")

  }

  if (ois4) {
    lines(
      x = as.Date(hist$dates),
      y = hist$ois4,
      type = "l",
      lwd = 3,
      col = "red"
    )

    chartLegend <- append(chartLegend, "USD OIS 4M")
    chartLty <- append(chartLty, 1)
    chartLwd <- append(chartLwd, 3)
    chartCol <- append(chartCol, "red")

  }

  legend(
    "bottomright",
    legend = chartLegend,
    lty = chartLty,
    lwd = chartLwd,
    col = chartCol,
    bg = "transparent",
    bty = "n",
    horiz = FALSE
  )

  cat("DONE!!!")

}


