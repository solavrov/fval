#' Constructor of simple fixed income bond object
#'
#' Attributes:
#' $name - name as you wish
#' $isin - isin code
#' $currency - currency i.e. USD, RUB...
#' $faceAmount - face amount
#' $couponFreq - coupon frequency i.e. 2 for semiannual etc
#' $issueDate - issue date
#' $maturity - maturity date
#' $formula - yield formular where STD stands for standard convention, OFZ stands for OFZ convention
#' $dayCounter - day counter from dayCounter
#' $cfactor - conversion factor for T-bonds-notes for T-futures
#' $couponDates - vector of coupon dates
#' $couponAmounts - vector of coupon amounts
#'
#' @param file Name of csv file that contains bond attributes.
#' Heads should have names of attributes.
#' File should be located in fval_data folder. Just name without extension
#' @param dateFormat File date format from lubridate package i.e. "dmy", "mdy" etc
#' @param sep Separator i.e. comma, semicolon or else
#'
#' @return FIBond object
#' @export
FIBond <- function(file = NA,
                   dateFormat = "mdy",
                   sep = ",") {

  bond <- list()
  class(bond) <- "FIBond"

  #default attributes
  bond$name <- NA
  bond$isin <- NA
  bond$currency <- NA
  bond$initialFace <- NA
  bond$couponFreq <- NA
  bond$issueDate <- NA
  bond$maturity <- NA
  bond$formula <- "STD"
  bond$dayCounter <- dayCounter$ActualActual
  bond$cfactor <- NA

  bond$couponDates <- NA
  bond$couponAmounts <- NA
  bond$faceAmounts <- NA

  if (!is.na(file)) {

    file <- paste0("fval_data/", file, ".csv")

    if (file.exists(file)) {
      df <- read.csv(file, sep = sep)

      if (!is.null(df$name))
        bond$name <- as.character(df$name[1])
      if (!is.null(df$isin))
        bond$isin <- as.character(df$isin[1])
      if (!is.null(df$currency))
        bond$currency <- as.character(df$currency[1])

      if (!is.null(df$issueDate))
        bond$issueDate <-
          as.Date(lubridate::parse_date_time(as.character(df$issueDate[1]), dateFormat))

      if (!is.null(df$formula))
        bond$formula <- as.character(df$formula[1])
      if (!is.null(df$dayCounter))
        bond$dayCounter <- df$dayCounter[1]
      if (!is.null(df$cfactor))
        bond$cfactor <- df$cfactor[1]

      if (!is.null(df$couponDates)) {
        bond$couponDates <-
          as.Date(lubridate::parse_date_time(as.character(df$couponDates), dateFormat))
        bond$maturity <- tail(bond$couponDates, 1)
        bond$couponFreq <-
          round(length(bond$couponDates) / as.numeric(bond$maturity - bond$issueDate) * 365)
      }

      if (!is.null(df$couponAmounts))
        bond$couponAmounts <- df$couponAmounts

      if (!is.null(df$faceAmounts)) {
        bond$faceAmounts <- df$faceAmounts
        bond$initialFace <- sum(df$faceAmounts)
      }

    }  else {
      stop("ERROR! ", file, " is not found")

    }

  }

  return (bond)

}


#' Print FIBond object
#'
#' @param bond FIBond object
#'
#' @return Info on screen
#' @export
print.FIBond <- function(bond) {

  cat("name:         ", bond$name, "\n")
  cat("isin:         ", bond$isin, "\n")
  cat("currency:     ", bond$currency, "\n")
  cat("initialFace:  ", bond$initialFace, "\n")
  cat("couponFreq:   ", bond$couponFreq, "\n")
  cat("issueDate:    ", as.character(bond$issueDate), "\n")
  cat("maturuty:     ", as.character(bond$maturity), "\n")
  cat("formula:      ", bond$formula, "\n")
  cat("dayCounter:   ", counterName(bond$dayCounter), "\n")
  cat("cfactor:      ", bond$cfactor, "\n\n")
  print(
    data.frame(
      couponDates = bond$couponDates,
      couponAmounts = bond$couponAmounts,
      faceAmounts = bond$faceAmounts
    )
  )

}


#' Calculate coupon time i.e. days passed over days in coupon period for FIBond object
#'
#' @param bond FIBond object
#' @param settleDate Calculation date
#'
#' @return Coupon time for FIBond object
#' @export
getCouponTime.FIBond <- function(bond, settleDate = nextBizDay()) {

  if (settleDate >= bond$issueDate && settleDate <= bond$maturity) {

    nextPaymentIndex <- which(bond$couponDates > settleDate)[1]
    nextDay <- bond$couponDates[nextPaymentIndex]

    if (nextPaymentIndex >= 2) {
      prevDay <- bond$couponDates[nextPaymentIndex - 1]
    } else {
      prevDay <- bond$issueDate
    }

    period <- countDays(prevDay, nextDay, bond$dayCounter)
    daysPassed <- countDays(prevDay, settleDate, bond$dayCounter)

    time <- daysPassed / period

  } else {

    time <- NA

  }

  return (time)

}


#' Return current face amount
#'
#' @param bond FIBond object
#' @param settleDate Calculation date
#'
#' @return Current face amount
#' @export
getFace.FIBond <- function(bond, settleDate = nextBizDay()) {

    if (settleDate >= bond$issueDate && settleDate <= bond$maturity) {

      face <- sum(bond$faceAmounts[bond$couponDates > settleDate])

    } else {

      face <- NA

    }

  return (face)

}


#' Calculate accrued interest for FIBond object
#'
#' @param bond FIBond object
#' @param settleDate Calculation date
#'
#' @return Accrued interest
#' @export
getAccruedValue.FIBond <- function(bond, settleDate = nextBizDay()) {

  if (settleDate >= bond$issueDate && settleDate <= bond$maturity) {

    nextPaymentIndex <- which(bond$couponDates > settleDate)[1]
    coupon <- bond$couponAmounts[nextPaymentIndex]
    accrued <- coupon * getCouponTime.FIBond(bond, settleDate)

    if (bond$formula == "OFZ") {
      accrued <-
        round(accrued / bond$initialFace * 1000, digits = 2) * bond$initialFace / 1000
    }

  } else {

    accrued <- NA

  }

  return (accrued)

}


#' Calculate accrued interest in percentage of current face
#'
#' @param bond FIBond object
#' @param settleDate Calculation date
#'
#' @return Accrued interest in percentage of current face
#' @export
getAccrued.FIBond <- function(bond, settleDate = nextBizDay()) {
    getAccruedValue.FIBond(bond, settleDate) / getFace.FIBond(bond, settleDate) * 100
  }


#' Calculate value of FIBond object
#'
#' @param bond FIBond object
#' @param yield FIBond yield in percentage
#' @param settleDate Calculation date
#'
#' @return Dirty value of bond object
#' @export
getValue.FIBond <- function(bond, yield, settleDate = nextBizDay()) {

  if (settleDate >= bond$issueDate && settleDate <= bond$maturity) {

    spans <- as.numeric(bond$couponDates - settleDate)

    if (bond$formula == "OFZ") {

      factors <- (spans > 0) * 1 / (1 + yield / 100) ^ (spans / 365)
      payments <- bond$couponAmounts + bond$faceAmounts

    } else {

      numOfFutureCoupons <- length(spans[spans > 0])

      factors <- 1 / (1 + yield / 100 / bond$couponFreq) ^
        (1:numOfFutureCoupons - getCouponTime.FIBond(bond, settleDate))

      payments <- tail(bond$couponAmounts + bond$faceAmounts, numOfFutureCoupons)

    }

    value <- sum(payments * factors)

  } else {

    value <- NA

  }

  return (value)

}


#' Calculate clean price of FIBond
#'
#' @param bond FIBond object
#' @param yield FIBond yield in percentage
#' @param settleDate Calculation date
#'
#' @return Clean price of FIBond object in percentage
#' @export
getPrice.FIBond <- function(bond, yield, settleDate = nextBizDay()) {

    price <-
      (getValue.FIBond(bond, yield, settleDate) - getAccruedValue.FIBond(bond, settleDate)) /
      getFace.FIBond(bond, settleDate) * 100

    return (price)

  }


#' Calculate yield of FIBond object
#'
#' @param bond FIBond object
#' @param price FIBond clean price in percentage
#' @param settleDate Calculation date
#' @param digits Accuracy as a number of digits after point
#'
#' @return FIBond yield in percentage
#' @export
getYield.FIBond <- function(bond,
                            price,
                            settleDate = nextBizDay(),
                            digits = 4,
                            yieldRange = c(-90, 90)) {

  if (settleDate >= bond$issueDate && settleDate <= bond$maturity) {

    f <- function(x) (getPrice.FIBond(bond, x, settleDate) - price)
    solution <- uniroot(f, yieldRange, tol = 10 ^ (-digits - 1))
    yield <- round(solution$root, digits)

  } else {

    yield <- NA

  }

  return (yield)

}


#' Calculate PVBP of FIBond object
#'
#' @param bond FIBond object
#' @param price FIBond clean price in percentage
#' @param settleDate Calculation date
#'
#' @return PVBP of FIBond object in percentage
#' @export
getPVBP.FIBond <- function(bond, price, settleDate = nextBizDay()) {

  bp <- 0.01
  yield <- getYield.FIBond(bond, price, settleDate)

  pvbp <- (getPrice.FIBond(bond, yield + bp) -
             getPrice.FIBond(bond, yield - bp)) / 2

  return (pvbp)

}


#' Calculate carry for FIBond object
#'
#' @param bond FIBond object (can be a list)
#' @param price FIBond clean price in percentage
#' @param settleDate1 Start date
#' @param settleDate2 End date
#' @param repoRate Funding term repo rate in percentage
#'
#' @return Carry for FIBond object
#' @export
getCarryValue.FIBond <- function(bond,
                                 price,
                                 settleDate1,
                                 settleDate2,
                                 repoRate) {

  inPlay <- which(bond$couponDates > settleDate1 & bond$couponDates <= settleDate2)

  payments <- bond$couponAmounts[inPlay] + bond$faceAmounts[inPlay]

  couponDates <- bond$couponDates[inPlay]

  carry <-
    getAccruedValue.FIBond(bond, settleDate2) - getAccruedValue.FIBond(bond, settleDate1) +
    sum(payments * (1 + repoRate / 100 * as.numeric(settleDate2 - couponDates) / 360)) -
    (
      price / 100 * getFace.FIBond(bond, settleDate1) +
        getAccruedValue.FIBond(bond, settleDate1)
    ) *
    repoRate / 100 * as.numeric(settleDate2 - settleDate1) / 360

  carry <- carry * if (settleDate2 >= settleDate1) 1 else NA

  return (carry)

}


#' Calculate carry for FIBond object in percentage of current face
#'
#' @param bond FIBond object (can be a list)
#' @param price FIBond clean price in percentage
#' @param settleDate1 Start date
#' @param settleDate2 End date
#' @param repoRate Funding term repo rate in percentage
#'
#' @return Carry for FIBond object in percentage of current face
#' @export
getCarry.FIBond <- function(bond,
                            price,
                            settleDate1,
                            settleDate2,
                            repoRate) {


  getCarryValue.FIBond(bond, price, settleDate1, settleDate2, repoRate) /
    getFace.FIBond(bond, settleDate1) * 100

}
