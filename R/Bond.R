
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
    if (!is.null(df$faceAmounts)) b$faceAmounts <- df$faceAmounts

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
  cat("couponFreq:   ", bond$couponFreq, "\n")
  cat("issueDate:    ", as.character(bond$issueDate), "\n")
  cat("formula:      ", bond$formula, "\n")
  cat("dayCounter:   ", bond$dayCounter, "\n")
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

