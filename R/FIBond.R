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
#' @param file Name of csv file that contains bond attributes (can be a vector).
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

  p <- list()

  for (i in 1:length(file)) {

    b <- list()
    class(b) <- "FIBond"

    #default attributes
    b$name <- NA
    b$isin <- NA
    b$currency <- NA
    b$initialFace <- NA
    b$couponFreq <- NA
    b$issueDate <- NA
    b$maturity <- NA
    b$formula <- "STD"
    b$dayCounter <- dayCounter$ActualActual
    b$cfactor <- NA

    b$couponDates <- NA
    b$couponAmounts <- NA
    b$faceAmounts <- NA

    if (!is.na(file[i])) {

      file[i] <- paste0("fval_data/", file[i], ".csv")

      if (file.exists(file[i])) {

        df <- read.csv(file[i], sep = sep)

        if (!is.null(df$name)) b$name <- as.character(df$name[1])
        if (!is.null(df$isin)) b$isin <- as.character(df$isin[1])
        if (!is.null(df$currency)) b$currency <- as.character(df$currency[1])

        if (!is.null(df$issueDate)) b$issueDate <-
            as.Date(lubridate::parse_date_time(as.character(df$issueDate[1]), dateFormat))

        if (!is.null(df$formula)) b$formula <- as.character(df$formula[1])
        if (!is.null(df$dayCounter)) b$dayCounter <- df$dayCounter[1]
        if (!is.null(df$cfactor)) b$cfactor <- df$cfactor[1]

        if (!is.null(df$couponDates)) {
          b$couponDates <-
            as.Date(lubridate::parse_date_time(as.character(df$couponDates), dateFormat))
          b$maturity <- tail(b$couponDates, 1)
          b$couponFreq <-
            round(length(b$couponDates) / as.numeric(b$maturity - b$issueDate) * 365)
        }

        if (!is.null(df$couponAmounts)) b$couponAmounts <- df$couponAmounts

        if (!is.null(df$faceAmounts)) {
          b$faceAmounts <- df$faceAmounts
          b$initialFace <- sum(df$faceAmounts)
        }

      }  else {

        cat("ERROR!", file[i], "is not found\n")

      }

    }

    p[[i]] <- b
    if (!is.na(b$name)) names(p)[i] <- b$name

  }

  if (length(p) == 1)
    return (p[[1]])
  else
    return (p)

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
#' @param bond FIBond object (can be a list)
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Coupon time for FIBond object
#' @export
getCouponTime.FIBond <- function(bond, settleDate = nextBizDay()) {

  len <- checkParams(bond, settleDate)

  time <- numeric()

  for (i in 1:len) {

    b <- e(bond, i)
    date <- e(settleDate, i)

    if (date >= b$issueDate && date <= b$maturity) {

      nextPaymentIndex <- which(b$couponDates > date)[1]
      nextDay <- b$couponDates[nextPaymentIndex]

      if (nextPaymentIndex >= 2) {
        prevDay <- b$couponDates[nextPaymentIndex - 1]
      } else {
        prevDay <- b$issueDate
      }

      period <- RQuantLib::dayCount(prevDay, nextDay, b$dayCounter)
      daysPassed <- RQuantLib::dayCount(prevDay, date, b$dayCounter)

      time[i] <- daysPassed / period

    } else {

      time[i] <- NA

    }

  }

  return (time)

}


#' Return current face amount
#'
#' @param bond FIBond object (can be a list)
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Current face amount
#' @export
getFace.FIBond <- function(bond, settleDate = nextBizDay()) {

  len <- checkParams(bond, settleDate)

  face <- numeric()

  for (i in 1:len) {

    b <- e(bond, i)
    date <- e(settleDate, i)

    if (date >= b$issueDate && date <= b$maturity) {

      face[i] <- sum(b$faceAmounts[b$couponDates > date])

    } else {

      face[i] <- NA

    }

  }

  return (face)

}


#' Calculate accrued interest for FIBond object
#'
#' @param bond FIBond object (can be a list)
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Accrued interest
#' @export
getAccruedValue.FIBond <- function(bond, settleDate = nextBizDay()) {

  len <- checkParams(bond, settleDate)

  accrued <- numeric()

  for (i in 1:len) {

    b <- e(bond, i)
    date <- e(settleDate, i)

    if (date >= b$issueDate && date <= b$maturity) {

      nextPaymentIndex <- which(b$couponDates > date)[1]
      coupon <- b$couponAmounts[nextPaymentIndex]
      accrued[i] <- coupon * getCouponTime.FIBond(b, date)

      if (b$formula == "OFZ") {
        accrued[i] <-
          round(accrued[i] / b$initialFace * 1000, digits = 2) * b$initialFace / 1000
      }

    } else {

      accrued[i] <- NA

    }

  }

  return (accrued)

}


#' Calculate accrued interest in percentage of current face
#'
#' @param bond FIBond object (can be a list)
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Accrued interest in percentage of current face
#' @export
getAccrued.FIBond <- function(bond, settleDate = nextBizDay()) {
    getAccruedValue.FIBond(bond, settleDate) / getFace.FIBond(bond, settleDate) * 100
  }


#' Calculate value of FIBond object
#'
#' @param bond FIBond object (can be a list)
#' @param yield FIBond yield in percentage (can be a vector)
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Dirty value of bond object
#' @export
getValue.FIBond <- function(bond, yield, settleDate = nextBizDay()) {

  len <- checkParams(bond, yield, settleDate)

  value <- numeric()

  for (i in 1:len) {

    b <- e(bond, i)
    y <- e(yield, i)
    date <- e(settleDate, i)

    if (date >= b$issueDate && date <= b$maturity) {

      spans <- as.numeric(b$couponDates - date)

      if (b$formula == "OFZ") {

        factors <- (spans > 0) * 1 / (1 + y / 100) ^ (spans / 365)
        payments <- b$couponAmounts + b$faceAmounts

      } else {

        numOfFutureCoupons <- length(spans[spans > 0])

        factors <- 1 / (1 + y / 100 / b$couponFreq) ^
          (1:numOfFutureCoupons - getCouponTime.FIBond(b, date))

        payments <- tail(b$couponAmounts + b$faceAmounts, numOfFutureCoupons)

      }

      value[i] <- sum(payments * factors)

    } else {

      value[i] <- NA

    }

  }

  return (value)

}


#' Calculate clean price of FIBond
#'
#' @param bond FIBond object (can be a list)
#' @param yield FIBond yield in percentage (can be a vector)
#' @param settleDate Calculation date (can be a vector)
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
#' @param bond FIBond object (can be a list)
#' @param price FIBond clean price in percentage (can be a vector)
#' @param settleDate Calculation date (can be a vector)
#' @param digits Accuracy as a number of digits after point
#'
#' @return FIBond yield in percentage
#' @export
getYield.FIBond <- function(bond,
                            price,
                            settleDate = nextBizDay(),
                            digits = 4,
                            yieldRange = c(-90, 90)) {

  yield <- numeric()

  len <- checkParams(bond, price, settleDate)

  for (i in 1:len) {

    b <- e(bond, i)
    p <- e(price, i)
    date <- e(settleDate, i)

    if (date >= b$issueDate && date <= b$maturity) {

      f <- function(x) (getPrice.FIBond(b, x, date) - p)
      solution <- uniroot(f, yieldRange, tol = 10 ^ (-digits - 1))
      yield[i] <- round(solution$root, digits)

    } else {

      yield[i] <- NA

    }

  }

  return (yield)

}


#' Calculate PVBP of FIBond object
#'
#' @param bond FIBond object (can be a list)
#' @param price FIBond clean price in percentage (can be a vector)
#' @param settleDate Calculation date (can be a vector)
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
#' @param price FIBond clean price in percentage (can be a vector)
#' @param settleDate1 Start date (can be a vector)
#' @param settleDate2 End date (can be a vector)
#' @param repoRate Funding term repo rate in percentage (can be a vector)
#'
#' @return Carry for FIBond object
#' @export
getCarryValue.FIBond <- function(bond,
                                 price,
                                 settleDate1,
                                 settleDate2,
                                 repoRate) {

  len <- checkParams(bond, price, settleDate1, settleDate2, repoRate)

  carry <- numeric()

  for (i in 1:len) {

    b <- e(bond, i)
    p <- e(price, i)
    date1 <- e(settleDate1, i)
    date2 <- e(settleDate2, i)
    rp <- e(repoRate, i)

    inPlay <- which(b$couponDates > date1 & b$couponDates <= date2)

    payments <- b$couponAmounts[inPlay] + b$faceAmounts[inPlay]

    couponDates <- b$couponDates[inPlay]

    carry[i] <-
      getAccruedValue.FIBond(b, date2) - getAccruedValue.FIBond(b, date1) +
      sum(payments * (1 + rp / 100 * as.numeric(date2 - couponDates) / 360)) -
      (
        p / 100 * getFace.FIBond(b, date1) +
          getAccruedValue.FIBond(b, date1)
      ) *
      rp / 100 * as.numeric(date2 - date1) / 360

  }

  return (carry)

}


#' Calculate carry for FIBond object in percentage of current face
#'
#' @param bond FIBond object (can be a list)
#' @param price FIBond clean price in percentage (can be a vector)
#' @param settleDate1 Start date (can be a vector)
#' @param settleDate2 End date (can be a vector)
#' @param repoRate Funding term repo rate in percentage (can be a vector)
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
