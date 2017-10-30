
#' Return contract type by TFutures ticker
#'
#' @param ticker Ticker (can be a vector)
#'
#' @return Contract type
#' @export
getContractType.TFutures <- function(ticker) {

  vectorSwitch(
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

  vectorSwitch(getFuturesCodeFromTicker(ticker),
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

  for (i in 1:3) thirdNextMonthBizDay <- nextBizDay(thirdNextMonthBizDay,
                                                    calendar = "UnitedStates/GovernmentBond")

  deliveryDate <- matrixSwitch(
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
#' @param ticker Bloomberg ticker like TYU7 etc
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
        ctdFileName <- paste0("ctd_", tolower(ticker))
        if (!file.exists(paste0("fval_data/", ctdFileName, ".csv"))) {
          ctdFileName <- ""
          cat("Can't find CTD's file in fval_data", ctdFileName, "\nPlease, set up CTD manually...\n\n")
        }
      }

    } else {
      cat("Ticker is wrong. Default contract was created\n")
    }

  }

  if (ctdFileName != "") fut$ctd <- FIBond(ctdFileName, dateFormat)

  return (fut)

}


#' Calculate model price of TFutures object
#'
#' @param fut TFutures object
#' @param ctdPrice CTD bond clean price (can be a vector)
#' @param repoRate Term CTD repo rate (can be a vector)
#' @param tradeDate Calculation date (can be a vector)
#'
#' @return Model price of TFutures object where 1 is 100\% of notional
#' @export
getPrice.TFutures <- function(fut, ctdPrice, repoRate, tradeDate = Sys.Date()) {

  t1 <- nextBizDay(tradeDate, calendar = "UnitedStates/GovernmentBond")
  carry <- getCarryPrice.FIBond(fut$ctd, ctdPrice, t1, fut$deliveryDate, repoRate)
  price <- (ctdPrice - carry) / fut$ctd$cfactor

  return (price)

}


#' Calculate implied repo rate for TFutures object
#'
#' @param fut TFutures object
#' @param futPrice TFutures price (can be a vector)
#' @param ctdPrice CTD bond clean price (can be a vector)
#' @param tradeDate Calculation date (can be a vector)
#'
#' @return Implied repo rate for TFutures object
#' @export
getIRP.TFututes <- function(fut, futPrice, ctdPrice, tradeDate = Sys.Date()) {

  t1 <- nextBizDay(tradeDate, calendar = "UnitedStates/GovernmentBond")

  inPlay <- which(fut$ctd$couponDates > t1 & fut$ctd$couponDates <= fut$deliveryDate)

  couponAmounts <- fut$ctd$couponAmounts[inPlay] / fut$ctd$initialFace
  couponDates <- fut$ctd$couponDates[inPlay]

  irp <-
    (
      fut$ctd$cfactor * futPrice + getAccruedPrice.FIBond(fut$ctd, fut$deliveryDate)
      + sum(couponAmounts) - ctdPrice - getAccruedPrice.FIBond(fut$ctd, t1)
    ) /
    (
      (ctdPrice + getAccruedPrice.FIBond(fut$ctd, t1)) * as.numeric(fut$deliveryDate - t1) / 360 -
       sum(couponAmounts * as.numeric(fut$deliveryDate - couponDates) / 360)
    )

  return (irp)

}


#' Calculate PVBP of TFutures object relative to CTD yield change
#'
#' @param fut TFutures object
#' @param ctdPrice CTD bond clean price (can be a vector)
#' @param repoRate CTD repo rate (can be a vector)
#' @param tradeDate Calculation date (can be a vector)
#'
#' @return PVBP of TFutures object relative to CTD yield change
#' @export
getPVBP.TFutures <- function(fut, ctdPrice, repoRate, tradeDate = Sys.Date()) {

  bp <- 1e-4
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
#' @param ctdPrice CTD bond clean price (can be a vector)
#' @param repoRate CTD repo rate (can be a vector)
#' @param tradeDate Calculation date (can be a vector)
#'
#' @return PVBP of TFutures object relative to CTD repo rate change
#' @export
getPVBPRP.TFutures <- function(fut, ctdPrice, repoRate, tradeDate = Sys.Date()) {

  bp <- 1e-4

  pvbp <- (
    getPrice.TFutures(fut, ctdPrice, repoRate + bp, tradeDate) -
      getPrice.TFutures(fut, ctdPrice, repoRate - bp, tradeDate)
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

  impliedRates <- hlpr::KVVV(getIRP.TFututes,
                             fut,
                             hist[[futField]],
                             hist[[ctdField]],
                             hist$dates)

  modelPrices <-
    hlpr::KVVV(getPrice.TFutures, fut, hist[[ctdField]], hist[[repoField]], hist$dates)

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
#' @param ticker Ticker like TYU7 etc
#' @param ois3 Chart errors for USD OIS 3M as a term repo rate if TRUE
#' @param ois4 Chart errors for USD OIS 4M as a term repo rate if TRUE
#'
#' @return Chart of TFutures pricing model errors
#' @export
demoTFutModelError <- function(ticker, ois3 = FALSE, ois4 = FALSE) {

  cat("ATTENTION: Need demo data files in fval_data folder\n")

  fut <- TFutures(ticker)
  histFile <- paste0("fval_data/hist_", tolower(ticker), ".csv")

  cat("Charting", fut$name, fut$ticker, "...\n")

  analytBloom <- getTFutModelAnalytics(fut, histFile)
  analytOis3 <- getTFutModelAnalytics(fut, histFile, repoField = "ois3")
  analytOis4 <- getTFutModelAnalytics(fut, histFile, repoField = "ois4")

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
#' @param ticker Ticker like TYU7 etc
#'
#' @return Chart of PVBP relative to term repo rate over time
#' @export
demoTFutSensToRP <- function(ticker) {

  cat("ATTENTION: Need demo data files in fval_data folder\n")

  fut <- TFutures(ticker)
  hist <- read.csv(paste0("fval_data/hist_", tolower(ticker), ".csv"))
  hist$dates <- as.Date(lubridate::parse_date_time(as.character(hist$dates), "ymd"))

  pvbprp <- hlpr::KKKV(getPVBPRP.TFutures,
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


