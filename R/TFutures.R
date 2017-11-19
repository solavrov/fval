
#' Return contract type by TFutures ticker
#'
#' @param ticker Ticker
#'
#' @return Contract type
#' @export
getContractType.TFutures <- function(ticker) {

  switch(
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
#' @param ticker Ticker
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

  if (is.na(type)) name <- NA

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

  for (i in 1:3) thirdNextMonthBizDay <-
    nextBizDay(thirdNextMonthBizDay, calendar = "UnitedStates/GovernmentBond")

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
#' $notional - notional amount of futures
#' $deliveryDate - Last delivery date that is supposed to be an accual delivary date
#' $ctd - Ceapest-to-deliver Bond object
#'
#' @param ticker Bloomberg ticker like TYU7 etc
#' @param ctdFile Name of ctd data file
#' @param dateFormat File date format from lubridate package i.e. "dmy", "mdy" etc
#'
#' @return TFutures object
#' @export
TFutures <- function(ticker = NA,
                     ctdFile = "",
                     dateFormat = "mdy",
                     decade = "auto") {

    fut <- list()
    class(fut) <- "TFutures"

    #default attributes
    fut$name <- NA
    fut$ticker <- NA
    fut$notional <- NA
    fut$deliveryDate <- NA
    fut$ctd <- NA

    #attributes by ticker
    if (!is.na(ticker))  {

      if (!is.na(fut$name <- getName.TFutures(ticker, decade))) {

        fut$ticker <- ticker
        fut$deliveryDate <- getDeliveryDate.TFutures(ticker, decade)
        fut$notional <- getNotional.TFutures(ticker)

        if (ctdFile == "") {
          ctdFile <- paste0("ctd_", tolower(ticker))
          path <- paste0("fval_data/", ctdFile, ".csv")
          if (!file.exists(path)) {
            ctdFile <- ""
            cat("WARNING!", path, "is not found\n")
          }
        }

      } else {
        cat("WARNING! Ticker", ticker, "is wrong!\n")
      }

    }

    if (ctdFile != "") fut$ctd <- FIBond(ctdFile, dateFormat)

    return (fut)

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

  value <- fut$notional * (futPrice - settlePrice) / 100
  if (side != "long") value <- -value

  return (value)

}


#' Return percentage carry of FIBond till delivery date of TFutures
#'
#' @param fut TFutures object
#' @param bondPrice FIBond price in percentage
#' @param repoRate FIBond repo rate in percentage
#' @param tradeDate Trade date
#' @param bond FIBond object
#'
#' @return Carry in percentage of FIBond face
#' @export
getCarry.TFututes <- function(fut,
                              bondPrice,
                              repoRate,
                              tradeDate = Sys.Date(),
                              bond = fut$ctd) {

  getCarry.FIBond(
    bond,
    bondPrice,
    nextBizDay(tradeDate, calendar = "UnitedStates/GovernmentBond"),
    fut$deliveryDate,
    repoRate
  )

}


#' Calculate model price of TFutures object
#'
#' @param fut TFutures object
#' @param ctdPrice CTD bond clean price in percentage
#' @param repoRate Term CTD repo rate in percentage
#' @param tradeDate Calculation date
#'
#' @return Model price of TFutures object in percentage of notional
#' @export
getPrice.TFutures <- function(fut, ctdPrice, repoRate, tradeDate = Sys.Date()) {
  (ctdPrice - getCarry.TFututes(fut, ctdPrice, repoRate, tradeDate)) /
    fut$ctd$cfactor
}


#' Calculate implied repo rate for TFutures object
#'
#' @param fut TFutures object
#' @param futPrice TFutures price in percentage
#' @param bondPrice CTD bond clean price in percentage
#' @param tradeDate Calculation date
#' @param bond FIBond object
#'
#' @return Implied repo rate for TFutures object in percentage
#' @export
getIRP.TFututes <- function(fut,
                            futPrice,
                            bondPrice,
                            tradeDate = Sys.Date(),
                            bond = fut$ctd) {

  t1 <- nextBizDay(tradeDate, calendar = "UnitedStates/GovernmentBond")

  inPlay <- which(bond$couponDates > t1 & bond$couponDates <= fut$deliveryDate)

  couponAmounts <- bond$couponAmounts[inPlay] / bond$initialFace * 100
  couponDates <- bond$couponDates[inPlay]

  irp <-
    (
      bond$cfactor * futPrice + getAccrued.FIBond(bond, fut$deliveryDate)
      + sum(couponAmounts) - bondPrice - getAccrued.FIBond(bond, t1)
    ) /
    (
      (bondPrice + getAccrued.FIBond(bond, t1)) *
        as.numeric(fut$deliveryDate - t1) / 360 -
        sum(
          couponAmounts * as.numeric(fut$deliveryDate - couponDates) / 360
        )
    ) * 100

  return (irp)

}


#' Calculate PVBP of TFutures object relative to CTD yield change
#'
#' @param fut TFutures object
#' @param ctdPrice CTD bond clean price in percentage
#' @param repoRate CTD repo rate in percentage
#' @param tradeDate Calculation date
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
#' @param ctdPrice CTD bond clean price in percentage
#' @param repoRate CTD repo rate in percentage
#' @param tradeDate Calculation date
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
#' @param futPrice TFutures price in percentage
#' @param bondPrice FIBond price in percentage
#'
#' @return Basis in percentage of FIBond face
#' @export
getBasis.TFutures <- function(fut, futPrice, bondPrice) {
  bondPrice - fut$ctd$cfactor * futPrice
}


#' Return Net Basis for TFutures object and given FIBond
#'
#' @param fut TFutures object
#' @param futPrice TFutures price in percentage
#' @param bondPrice FIBond price in percentage
#' @param repoRate FIBond repo rate in percentage
#' @param bond FIBond object
#' @param tradeDate Trade date
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


