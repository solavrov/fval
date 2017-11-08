
#' Return contract type by TFutures ticker
#'
#' @param ticker Ticker (can be a vector)
#'
#' @return Contract type
#' @export
getContractType.TFutures <- function(ticker) {

  hlpr::vectorSwitch(
    getFuturesCodeFromTicker(ticker),
    TU = "2Y TNote",
    "3Y" = "3Y TNote",
    FV = "5Y TNote",
    TY = "10Y TNote",
    UXY = "Ultra 10Y TNote",
    US = "TBond",
    WN = "Ultra TBond",
    NA
  )

}


#' Return contract name by TFutures ticker
#'
#' @param ticker Ticker (can be a vector)
#' @param decade decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return Contract name
#' @export
getName.TFutures <- function(ticker, decade = "auto") {

  name <- NA

  type <- getContractType.TFutures(ticker)

  month <- month.abb[getMonthNumberFromFuturesTicker(ticker)]
  year <- getYearFromFuturesTicker(ticker, decade)
  name <- paste0(type, ' ', month, '-', year)

  for (i in 1:length(ticker)) if (is.na(type[i])) name[i] <- NA

  return (name)

}


#' Return notional amount by TFutures ticker
#'
#' @param ticker Ticker (can be a vector)
#'
#' @return Notional amount
#' @export
getNotional.TFutures <- function(ticker) {

  hlpr::vectorSwitch(getFuturesCodeFromTicker(ticker),
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
#' @param ticker Ticker (can be a vector)
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

  for (i in 1:3) thirdNextMonthBizDay <-
    nextBizDay(thirdNextMonthBizDay, calendar = "UnitedStates/GovernmentBond")

  deliveryDate <- hlpr::matrixSwitch(
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
#' @param ticker Bloomberg ticker like TYU7 etc (can be a vector)
#' @param ctdFile Name of ctd data file (can be a vector)
#' @param dateFormat File date format from lubridate package i.e. "dmy", "mdy" etc
#'
#' @return TFutures object
#' @export
TFutures <- function(ticker = NA, ctdFile = "", dateFormat = "mdy", decade = "auto") {

  l <- list()

  len <- checkParams(ticker, ctdFile)

  for (i in 1:len) {

    tkr <- e(ticker, i)
    file <- e(ctdFile, i)

    f <- list()
    class(f) <- "TFutures"

    #default attributes
    f$name <- NA
    f$ticker <- NA
    f$notionalAmount <- NA
    f$deliveryDate <- NA
    f$ctd <- NA

    #attributes by ticker
    if (!is.na(tkr))  {

      if (!is.na(f$name <- getName.TFutures(tkr, decade))) {

        f$ticker <- tkr
        f$deliveryDate <- getDeliveryDate.TFutures(tkr, decade)
        f$notionalAmount <- getNotional.TFutures(tkr)

        if (file == "") {
          file <- paste0("ctd_", tolower(tkr))
          path <- paste0("fval_data/", file, ".csv")
          if (!file.exists(path)) {
            file <- ""
            cat("WARNING!", path, "is not found\n")
          }
        }

      } else {
        cat("WARNING! Ticker", tkr, "is wrong!\n")
      }

    }

    if (file != "") f$ctd <- FIBond(file, dateFormat)

    l[[i]] <- f
    if (!is.na(f$ticker)) names(l)[i] <- f$ticker

  }

  if (length(l) == 1)
    return (l[[1]])
  else
    return (l)

}


#' Return value of TFutures contract
#'
#' @param fut TFutures object
#' @param futPrice TFutures price in percentage
#' @param settlePrice TFutures last settlement price in percentage
#' @param side Side - "long" or "short"
#'
#' @return Value in dollars of one contract
#' @export
getValue.TFutures <- function(fut, futPrice, settlePrice, side = "long") {

  len <- checkParams(fut, futPrice, settlePrice, side)

  value <- numeric()

  for (i in 1:len) {

    fut.i <- e(fut, i)
    futPrice.i <- e(futPrice, i)
    settlePrice.i <- e(settlePrice, i)
    side.i <- e(side, i)

    value[i] <- fut.i$notionalAmount * (futPrice.i - settlePrice.i) / 100
    if (side.i != "long") value[i] <- -value[i]

  }

  return (value)

}


#' Return percentage carry of FIBond till delivery date of TFutures
#'
#' @param fut TFutures object
#' @param bondPrice FIBond price in percentage (can be a vector)
#' @param repoRate FIBond repo rate in percentage (can be a vector)
#' @param tradeDate Trade date (can be a vector)
#' @param bond FIBond object
#'
#' @return Carry in percentage of FIBond face
#' @export
getCarry.TFututes <- function(fut, bondPrice, repoRate, tradeDate = Sys.Date(), bond = fut$ctd) {

  getCarry.FIBond(
    bond,
    bondPrice,
    nextBizDay(tradeDate, calendar = "UnitedStates/GovernmentBond"),
    fut$deliveryDate,
    repoRate
  ) * checkDate(tradeDate, latestDate = fut$deliveryDate)

}


#' Calculate model price of TFutures object
#'
#' @param fut TFutures object
#' @param ctdPrice CTD bond clean price in percentage (can be a vector)
#' @param repoRate Term CTD repo rate in percentage (can be a vector)
#' @param tradeDate Calculation date (can be a vector)
#'
#' @return Model price of TFutures object in percentage of notional
#' @export
getPrice.TFutures <- function(fut, ctdPrice, repoRate, tradeDate = Sys.Date()) {

  (ctdPrice - getCarry.TFututes(fut, ctdPrice, repoRate, tradeDate)) / fut$ctd$cfactor

}


#' Calculate implied repo rate for TFutures object
#'
#' @param fut TFutures object
#' @param futPrice TFutures price in percentage (can be a vector)
#' @param bondPrice CTD bond clean price in percentage (can be a vector)
#' @param tradeDate Calculation date (can be a vector)
#' @param bond FIBond object (can be a list)
#'
#' @return Implied repo rate for TFutures object in percentage
#' @export
getIRP.TFututes <- function(fut, futPrice, bondPrice, tradeDate = Sys.Date(), bond = fut$ctd) {

  len <- checkParams(futPrice, bondPrice, tradeDate, bond)

  t1 <- nextBizDay(tradeDate, calendar = "UnitedStates/GovernmentBond")

  irp <- numeric()

  for (i in 1:len) {

    fprice <- e(futPrice, i)
    bprice <- e(bondPrice, i)
    date <- e(t1, i)
    b <- e(bond, i)

    inPlay <- which(b$couponDates > date & b$couponDates <= fut$deliveryDate)

    couponAmounts <- b$couponAmounts[inPlay] / b$initialFace * 100
    couponDates <- b$couponDates[inPlay]

    irp[i] <-
      (
        b$cfactor * fprice + getAccrued.FIBond(b, fut$deliveryDate)
        + sum(couponAmounts) - bprice - getAccrued.FIBond(b, date)
      ) /
      (
        (bprice + getAccrued.FIBond(b, date)) *
          as.numeric(fut$deliveryDate - date) / 360 -
          sum(couponAmounts * as.numeric(fut$deliveryDate - couponDates) / 360)
      ) * 100

  }

  return (irp)

}


#' Calculate PVBP of TFutures object relative to CTD yield change
#'
#' @param fut TFutures object
#' @param ctdPrice CTD bond clean price in percentage (can be a vector)
#' @param repoRate CTD repo rate in percentage (can be a vector)
#' @param tradeDate Calculation date (can be a vector)
#'
#' @return PVBP of TFutures object relative to CTD yield change in percentage
#' @export
getPVBP.TFutures <- function(fut, ctdPrice, repoRate, tradeDate = Sys.Date()) {

  bp <- 0.01
  t1 <- nextBizDay(tradeDate, calendar = "UnitedStates/GovernmentBond")
  ctdYield <- getYield.FIBond(fut$ctd, ctdPrice, t1)

  bpPlusCTDprice <- getPrice.FIBond(fut$ctd, ctdYield + bp, t1)
  bpMinusCTDprice <- getPrice.FIBond(fut$ctd, ctdYield - bp, t1)

  pvbp <-
    (
      getPrice.TFutures(fut, bpPlusCTDprice, repoRate, tradeDate) -
        getPrice.TFutures(fut, bpMinusCTDprice, repoRate, tradeDate)
    ) / 2

  return (pvbp)

}


#' Calculate PVBP of TFutures object relative to CTD repo rate change
#'
#' @param fut TFutures object
#' @param ctdPrice CTD bond clean price in percentage (can be a vector)
#' @param repoRate CTD repo rate in percentage (can be a vector)
#' @param tradeDate Calculation date (can be a vector)
#'
#' @return PVBP of TFutures object relative to CTD repo rate change in percentage
#' @export
getPVBPRP.TFutures <- function(fut, ctdPrice, repoRate, tradeDate = Sys.Date()) {

  bp <- 0.01

  pvbp <- (
    getPrice.TFutures(fut, ctdPrice, repoRate + bp, tradeDate) -
      getPrice.TFutures(fut, ctdPrice, repoRate - bp, tradeDate)
  ) / 2

  return (pvbp)

}


#' Return Basis for TFutures object and given FIBond
#'
#' @param fut TFutures object
#' @param futPrice TFutures price in percentage (can be a vector)
#' @param bondPrice FIBond price in percentage (can be a vector)
#'
#' @return Basis in percentage of FIBond face
#' @export
getBasis.TFutures <- function(fut, futPrice, bondPrice) {
  bondPrice - fut$ctd$cfactor * futPrice
}


#' Return Net Basis for TFutures object and given FIBond
#'
#' @param fut TFutures object
#' @param futPrice TFutures price in percentage (can be a vector)
#' @param bondPrice FIBond price in percentage (can be a vector)
#' @param repoRate FIBond repo rate in percentage (can be a vector)
#' @param bond FIBond object
#' @param tradeDate Trade date (can be a vector)
#'
#' @return Net Basis in percentage of FIBond face
#' @export
getNetBasis.TFutures <- function(fut,
                                 futPrice,
                                 bondPrice,
                                 repoRate,
                                 tradeDate = Sys.Date(),
                                 bond = fut$ctd) {

  getBasis.TFutures(fut, futPrice, bondPrice) -
    getCarry.TFututes(fut, bondPrice, repoRate, tradeDate, bond)

}


