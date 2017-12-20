
#' @export
FOLDER_TFUTURES <- "fval_data/TFutures/"

#' @export
TFUTURES_CF_YIELD <- 6


#' Return contract type by TFutures ticker
#'
#' @param ticker Ticker
#'
#' @return Contract type
#' @export
getType.TFutures <- function(ticker) {

  ticker <- toupper(ticker)

  code <- getCode.Futures(ticker)

  if (is.na(code))
    type <- NA
  else
    type <- switch(
      code,
      TU = "2Y TNote",
      "3Y" = "3Y TNote",
      FV = "5Y TNote",
      TY = "10Y TNote",
      UXY = "Ultra 10Y TNote",
      US = "TBond",
      WN = "Ultra TBond",
      NA
    )

  return (type)

}


#' Is TFutures ticker
#'
#' @param ticker Ticker
#'
#' @return TRUE if it is a ticker, FALSE otherwise
#' @export
isTicker.TFutures <- function(ticker) {
  isTicker.Futures(ticker) && !is.na(getType.TFutures(ticker))
}


#' Check TFutures ticker
#'
#' @param ticker Ticker
#'
#' @return Ticker if it is a ticker, NA otherwise
#' @export
checkTicker.TFutures <- function(ticker) {
  if (isTicker.TFutures(ticker))
    return (toupper(ticker))
  else
    return (NA)
}


#' Return contract name by TFutures ticker
#'
#' @param ticker Ticker
#' @param decade Decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return Contract name
#' @export
getName.TFutures <- function(ticker, decade = "auto") {

  type <- getType.TFutures(ticker)

  if (is.na(type)) {
    name <- NA
  } else {
    month <- month.abb[getMonth.Futures(ticker)]
    year <- getYear.Futures(ticker, decade)
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

  code <- getCode.Futures(ticker)

  if (is.na(code))
    notional <- NA
  else
    notional <- switch(
      code,
      TU = 2e5,
      "3Y" = 1e5,
      FV = 1e5,
      TY = 1e5,
      UXY = 1e5,
      US = 1e5,
      WN = 1e5,
      NA
    )

  return (notional)

}


#' Return first day of delivery month by TFutures ticker
#'
#' @param ticker Ticker
#' @param decade Decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return First day
#' @export
getFirstDay.TFutures <- function(ticker, decade = "auto") {

  if (isTicker.TFutures(ticker)) {
    fd <- firstDay(getMonth.Futures(ticker),
             getYear.Futures(ticker, decade))
  } else {
    fd <- NA
  }

  return (fd)

}


#' Return first intention date by TFutures ticker
#'
#' @param ticker Ticker
#' @param decade Decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return First intention date
#' @export
getFirstIntention.TFutures <- function(ticker, decade = "auto") {

  if (isTicker.TFutures(ticker))
    fi <- prevBizDay(prevBizDay(getFirstDay.TFutures(ticker, decade)))
  else
    fi <- NA

  return (fi)

}


#' Return model delivery date by TFutures ticker
#'
#' @param ticker Ticker
#' @param decade Decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return Delivery date
#' @export
getDeliveryDate.TFutures <- function(ticker, decade = "auto") {

  if (isTicker.TFutures(ticker)) {

    futuresCode <- getCode.Futures(ticker)

    lastMonthBizDay <- lastBizDay(getMonth.Futures(ticker),
                                  getYear.Futures(ticker, decade),
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

  } else {

    deliveryDate <- NA

  }

  return (deliveryDate)

}


#' Return last trading date by TFutures ticker
#'
#' @param ticker Ticker
#' @param decade Decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return Last trading date
#' @export
getLastTrading.TFutures <- function(ticker, decade = "auto") {

  if (isTicker.TFutures(ticker)) {

    futuresCode <- getCode.Futures(ticker)

    lastMonthBizDay <- lastBizDay(getMonth.Futures(ticker),
                                  getYear.Futures(ticker, decade),
                                  calendar = "UnitedStates/GovernmentBond")

    eighthMonthBizDayFromEnd <- lastMonthBizDay

    for (i in 1:7) eighthMonthBizDayFromEnd <-
      prevBizDay(eighthMonthBizDayFromEnd, calendar = "UnitedStates/GovernmentBond")

    lastTradingDate <- switch(
      futuresCode,
      TU = lastMonthBizDay,
      "3Y" = lastMonthBizDay,
      FV = lastMonthBizDay,
      TY = eighthMonthBizDayFromEnd,
      UXY = eighthMonthBizDayFromEnd,
      US = eighthMonthBizDayFromEnd,
      WN = eighthMonthBizDayFromEnd,
      NA
    )

  } else {

    lastTradingDate <- NA

  }

  return (lastTradingDate)

}


#' Return rounded bond's term to maturity according to CBOT rules
#'
#' @param maturity Maturity date
#' @param ticker TFutures ticker
#' @param decade Decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return Rounded bond's term in years
#' @export
getBondTerm.TFutures <- function(maturity, ticker, decade = "auto", forCFactor = FALSE) {

  firstDate <- getFirstDay.TFutures(ticker, decade)

  roundBy <- switch(
    getCode.Futures(ticker),
    TU = "month",
    "3Y" = "month",
    FV = "month",
    TY = "quarter",
    UXY = if (forCFactor) "quarter" else "month",
    US = "quarter",
    WN = "quarter",
    NA
  )

  term <- roundSpan(firstDate, maturity, roundBy = roundBy) /
    ((roundBy == "month") * 12 + (roundBy == "quarter") * 4)

  return (term)

}


#' Return covnersion factor for given FIBond and TFutures
#'
#' @param bond FIBond object
#' @param ticker Ticker of TFutures
#' @param decade Decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return Conversion factor
#' @export
getCFactor.TFutures <- function(bond, ticker, decade = "auto") {

  cf <- round(
    getPV.TVM(rate = TFUTURES_CF_YIELD / 2,
              nper = getBondTerm.TFutures(bond$maturity, ticker,
                                          decade = "auto", forCFactor = TRUE) * 2,
              pmt = bond$initialRate / 2 / 100,
              fv = 1,
              lessAccrued = TRUE),
    4)

  return (cf)

}


#' Return TRUE if FIBond is eligible for given ticker
#'
#' @param issueDate Issue date of FIBond
#' @param maturity Maturity date of FIBond
#' @param ticker Ticker of TFutures
#' @param decade Decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return TRUE if FIBond is form basket of TFutures, otherwise FALSE
#' @export
isEligible.TFutures <- function(issueDate, maturity, ticker, decade = "auto") {

  ti <- roundSpan(issueDate, maturity)
  tf <- getBondTerm.TFutures(maturity, ticker, decade) * 12

  answer <-
    switch(
     getCode.Futures(ticker),
     TU   = (ti <= 63)  && (tf >= 21)   && (tf <= 24),
     "3Y" = (ti <= 63)  && (tf >= 33)   && (tf <= 36),
     FV   = (ti <= 63)  && (tf >= 50),
     TY   = (ti <= 120) && (tf >= 78),
     UXY  = (ti <= 120) && (tf >= 113),
     US   =                (tf >= 180)  && (tf <= 300),
     WN   =                (tf >= 300),
     NA)

  return (answer)

}


#' Return TRUE if FIBond is eligible for given ticker
#'
#' @param bond FIBond object
#' @param ticker Ticker of TFutures
#' @param decade Decade that can be "auto" - default value, "pres" - present decade,
#' "prev" - previous decade, "next" - next decade
#'
#' @return TRUE if FIBond is form basket of TFutures, otherwise FALSE
#' @export
isEligible2.TFutures <- function(bond, ticker, decade = "auto") {
  isFromBasket(bond$issueDate, bond$maturity, ticker, decade)
}


#' Return ISINs of eligible FIBonds for giben ticker
#'
#' @param ticker Ticker of TFutures
#' @param folder FIBond's folder
#'
#' @return Vector of ISINs
#' @export
getEligibles.TFutures <- function(ticker, decade = "auto") {

  df <- dir.FIBond(folder = FOLDER_FIBONDS_US_GOV)
  whichBasket <- which(mapply(isEligible.TFutures, df$issueDate, df$maturity, ticker, decade))
  isins <- as.character(df$isin[whichBasket])

  return (isins)

}


#' Return FIBond object of CTD for a given TFutures
#'
#' @param ticker Ticker of TFutures
#' @param file Filename of CTD without extension
#'
#' @return FIBond object of CTD
#' @export
loadCTD.TFutures <- function(ticker, file = "") {

  if (file != "")
    ctd <- FIBond(file)
  else if (isTicker.TFutures(ticker))
    ctd <- FIBond(paste0("ctd_", ticker), folder = FOLDER_TFUTURES)
  else {
    if (!is.na(ticker)) stop("Ticker ", toupper(ticker), " is wrong")
    ctd <- NA
  }

  return (ctd)

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
TFutures <- function(ticker = NA, ctdFile = "", dateFormat = "mdy", decade = "auto") {

    fut <- list()
    class(fut) <- "TFutures"

    fut$name <- getName.TFutures(ticker, decade)
    fut$ticker <- checkTicker.TFutures(ticker)
    fut$notional <- getNotional.TFutures(ticker)
    fut$firstIntention <- getFirstIntention.TFutures(ticker, decade)
    fut$lastTrading <- getLastTrading.TFutures(ticker, decade)
    fut$deliveryDate <- getDeliveryDate.TFutures(ticker)
    fut$ctd <- loadCTD.TFutures(ticker, ctdFile)

    return (fut)

  }


#' Print TFutures object
#'
#' @param bond TFutures object
#'
#' @return Info on screen
#' @export
print.TFutures <- function(fut) {
  cat("name:           ", fut$name, "\n")
  cat("ticker:         ", fut$ticker, "\n")
  cat("notional:       ", prettyNum(f$notional, big.mark = ",", scientific = FALSE), "\n")
  cat("firstIntention: ", as.character(fut$firstIntention), "\n")
  cat("lastTrading:    ", as.character(fut$lastTrading), "\n")
  cat("deliveryDate:   ", as.character(fut$deliveryDate), "\n\n")
  cat("CTD info\n--------\n")
  print(fut$ctd)
}


#' Show attributes of all ctd bonds in FOLDER_TFUTURES
#'
#' @param attr Vector of attributes' names
#'
#' @return Data frame with bonds' attributes
#' @export
dir.TFutures <- function(attr = c("name", "isin", "issueDate", "maturity")) {
  dir.FIBond(FOLDER_TFUTURES, attr)
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


