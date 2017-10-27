
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
#' $dayCounter - day counter from dayCounter
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
  b$initialFace <- NA
  b$couponFreq <- NA
  b$issueDate <- NA
  b$formula <- "STD"
  b$dayCounter <- dayCounter$ActualActual
  b$cfactor <- NA

  b$couponDates <- NA
  b$couponAmounts <- NA
  b$faceAmounts <- NA


  if (!is.na(file)) {

    df <- read.csv(file, sep = sep)

    if (!is.null(df$name)) b$name <- as.character(df$name[1])
    if (!is.null(df$isin)) b$isin <- as.character(df$isin[1])
    if (!is.null(df$currency)) b$currency <- as.character(df$currency[1])
    if (!is.null(df$couponFreq)) b$couponFreq <- df$couponFreq[1]

    if (!is.null(df$issueDate))
      b$issueDate <- as.Date(lubridate::parse_date_time(as.character(df$issueDate[1]), dateFormat))

    if (!is.null(df$formula)) b$formula <- as.character(df$formula[1])
    if (!is.null(df$dayCounter)) b$dayCounter <- df$dayCounter[1]
    if (!is.null(df$cfactor)) b$cfactor <- df$cfactor[1]

    if (!is.null(df$couponDates))
      b$couponDates <- as.Date(lubridate::parse_date_time(as.character(df$couponDates), dateFormat))

    if (!is.null(df$couponAmounts)) b$couponAmounts <- df$couponAmounts

    if (!is.null(df$faceAmounts)) {
      b$faceAmounts <- df$faceAmounts
      b$initialFace <- sum(df$faceAmounts)
    }

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
  cat("initialFace:  ", bond$initialFace, "\n")
  cat("couponFreq:   ", bond$couponFreq, "\n")
  cat("issueDate:    ", as.character(bond$issueDate), "\n")
  cat("formula:      ", bond$formula, "\n")
  cat("dayCounter:   ", counterName(bond$dayCounter), "\n")
  cat("cfactor:      ", bond$cfactor, "\n\n")
  print(data.frame(
    couponDates = bond$couponDates,
    couponAmounts = bond$couponAmounts,
    faceAmounts = bond$faceAmounts
  ))
}


#' Calculate coupon time i.e. days passed over days in coupon period for Bond object
#'
#' @param bond Bond object
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Coupon time for Bond object
#' @export
getCouponTime.Bond.fval <- function(bond, settleDate = nextBizDay()) {

  time <- numeric()

  for (i in 1:length(settleDate)) {

    nextPaymentIndex <- which(bond$couponDates > settleDate[i])[1]
    nextDay <- bond$couponDates[nextPaymentIndex]

    if (nextPaymentIndex >= 2) {
      prevDay <- bond$couponDates[nextPaymentIndex - 1]
    } else {
      prevDay <- bond$issueDate
    }

    period <- RQuantLib::dayCount(prevDay, nextDay, bond$dayCounter)
    daysPassed <- RQuantLib::dayCount(prevDay, settleDate[i], bond$dayCounter)

    time[i] <- daysPassed / period

  }

  return (time)

}


#' Return current face amount
#'
#' @param bond Bond object
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Current face amount
#' @export
getCurrentFace.Bond.fval <- function(bond, settleDate = nextBizDay()) {

  face <- numeric()

  for (i in 1:length(settleDate)) {
    face[i] <- sum(bond$faceAmounts[bond$couponDates > settleDate[i]])
  }

  return (face)

}


#' Calculate accrued interest for Bond object
#'
#' @param bond Bond object
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Accrued interest
#' @export
getAccrued.Bond.fval <- function(bond, settleDate = nextBizDay()) {

  accrued <- numeric()

  for (i in 1:length(settleDate)) {

    nextPaymentIndex <- which(bond$couponDates > settleDate[i])[1]
    coupon <- bond$couponAmounts[nextPaymentIndex]
    accrued[i] <- coupon * getCouponTime.Bond.fval(bond, settleDate[i])

    if (bond$formula == "OFZ") {
      accrued[i] <- round(accrued[i] / bond$initialFace * 1000, digits = 2) *
        bond$initialFace / 1000
    }

  }

  return (accrued)

}


#' Calculate accrued interest for price of Bond
#'
#' @param bond Bond object
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Accrued interest in percentage of current face
#' @export
getAccruedForPrice.Bond.fval <- function(bond, settleDate = nextBizDay()) {
  getAccrued.Bond.fval(bond, settleDate) / getCurrentFace.Bond.fval(bond, settleDate)
}


#' Calculate value of Bond object
#'
#' @param bond Bond object
#' @param yield Bond yield (can be a vector)
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Dirty value of bond object
#' @export
getValue.Bond.fval <- function(bond, yield, settleDate = nextBizDay()) {

  yield <- stretch(yield, settleDate)
  settleDate <- stretch(settleDate, yield)
  len <- length(yield)

  value <- numeric()

  for (i in 1:len) {

    spans <- as.numeric(bond$couponDates - settleDate[i])

    if (bond$formula == "OFZ") {
      factors <- (spans > 0) * 1 / (1 + yield[i]) ^ (spans / 365)
      payments <- bond$couponAmounts + bond$faceAmounts

    } else {

      numOfFutureCoupons <- length(spans[spans > 0])

      factors <-
        1 / (1 + yield[i] / bond$couponFreq) ^
        (1:numOfFutureCoupons - getCouponTime.Bond.fval(bond, settleDate[i]))

      payments <-
        tail(bond$couponAmounts + bond$faceAmounts, numOfFutureCoupons)

    }

    value[i] <- sum(payments * factors)

  }

  return (value)

}


#' Calculate clean price of Bond
#'
#' @param bond Bond object
#' @param yield Bond yield (can be a vector)
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Clean price of Bond object where 1 is 100%
#' @export
getPrice.Bond.fval <- function(bond, yield, settleDate = nextBizDay()) {

  price <-
    (getValueOfBond(bond, yield, settleDate) - getAccrued.Bond.fval(bond, settleDate)) /
    getCurrentFace.Bond.fval(bond, settleDate)

  return (price)

}


#' Calculate yield of Bond object
#'
#' @param bond Bond object
#' @param price Bond clean price where 1 is 100% (can be a vector)
#' @param settleDate Calculation date (can be a vector)
#' @param digits Accuracy as a number of digits after point
#'
#' @return Bond yield
#' @export
getYield.Bond.fval <- function(bond,
                               price,
                               settleDate = nextBizDay(),
                               digits = 6) {

  yieldRange <- c(-0.9, 0.9)

  yield <- numeric()

  price <- stretch(price, settleDate)
  settleDate <- stretch(settleDate, price)
  len <- length(price)

  for (i in 1:len) {
    f <- function(x) (getPrice.Bond.fval(bond, x, settleDate[i]) - price[i])
    solution <- uniroot(f, yieldRange, tol = 10 ^ (-digits - 1))
    yield[i] <- round(solution$root, digits)
  }

  return (yield)

}


#' Calculate PVBP of Bond object
#'
#' @param bond Bond object
#' @param price Clean price of Bond object for 100 face (can be a vector)
#' @param settleDate Calculation date (can be a vector)
#'
#' @return PVBP of Bond object
#' @export
getPVBP.Bond.fval <- function(bond, price, settleDate = nextBizDay()) {

  bp <- 10^-4
  yield <- getYield.Bond.fval(bond, price, settleDate)

  pvbp <- (getPrice.Bond.fval(bond, yield + bp) -
             getPrice.Bond.fval(bond, yield - bp)) / 2

  return (pvbp)

}


#' Calculate carry for Bond object for 100 face
#'
#' @param bond Bond object
#' @param price Clean price of Bond object for 100 face (can be a vector)
#' @param settleDate1 Start date (can be a vector)
#' @param settleDate2 End date (can be a vector)
#' @param repoRate Funding term repo rate (can be a vector)
#'
#' @return Carry for Bond object
#' @export
getCarryForPrice.Bond.fval <- function(bond,
                                       price,
                                       settleDate1,
                                       settleDate2,
                                       repoRate) {

  price <- stretch(price, settleDate1, settleDate2, repoRate)
  settleDate1 <- stretch(settleDate1, price, settleDate2, repoRate)
  settleDate2 <- stretch(settleDate2, price, settleDate1, repoRate)
  repoRate <- stretch(repoRate, price, settleDate1, settleDate2)
  len <- length(price)

  carry <- numeric()

  for (i in 1:len) {

    inPlay <- which(bond$couponDates > settleDate1[i] & bond$couponDates <= settleDate2[i])

    couponAmounts <- bond$couponAmounts[inPlay] / bond$faceAmount * 100

    couponDates <- bond$couponDates[inPlay]

    carry <-
      getAccruedForPrice.Bond.fval(bond, settleDate2) -  getAccruedForPrice.Bond.fval(bond, settleDate1) +
      sum(couponAmounts * (1 + repoRate * as.numeric(settleDate2 - couponDates) / 360)) -
      (price + getAccruedForPrice.Bond.fval(bond, settleDate1)) *
      repoRate * as.numeric(settleDate2 - settleDate1) / 360

  }



  return (carry)

}

