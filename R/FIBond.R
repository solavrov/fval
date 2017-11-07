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
#' @param file Name of csv file that contains bond attributes. Heads should have names of attributes.
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
  class(b) <- "FIBond"

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
#' @param bond FIBond object
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Coupon time for FIBond object
#' @export
getCouponTime.FIBond <- function(bond, settleDate = nextBizDay()) {

  time <- numeric()

  for (i in 1:length(settleDate)) {

    if (settleDate[i] >= bond$issueDate && settleDate[i] <= bond$maturity) {

      nextPaymentIndex <- which(bond$couponDates > settleDate[i])[1]
      nextDay <- bond$couponDates[nextPaymentIndex]

      if (nextPaymentIndex >= 2) {
        prevDay <- bond$couponDates[nextPaymentIndex - 1]
      } else {
        prevDay <- bond$issueDate
      }

      period <- RQuantLib::dayCount(prevDay, nextDay, bond$dayCounter)
      daysPassed <-
        RQuantLib::dayCount(prevDay, settleDate[i], bond$dayCounter)

      time[i] <- daysPassed / period

    } else {

      time[i] <- NA

    }

  }

  return (time)

}


#' Return current face amount
#'
#' @param bond FIBond object
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Current face amount
#' @export
getCurrentFace.FIBond <- function(bond, settleDate = nextBizDay()) {

  face <- numeric()

  for (i in 1:length(settleDate)) {

    if (settleDate[i] >= bond$issueDate && settleDate[i] <= bond$maturity) {

      face[i] <- sum(bond$faceAmounts[bond$couponDates > settleDate[i]])

    } else {

      face[i] <- NA

    }

  }

  return (face)

}


#' Calculate accrued interest for FIBond object
#'
#' @param bond FIBond object
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Accrued interest
#' @export
getAccruedValue.FIBond <- function(bond, settleDate = nextBizDay()) {

  accrued <- numeric()

  for (i in 1:length(settleDate)) {

    if (settleDate[i] >= bond$issueDate && settleDate[i] <= bond$maturity) {

    nextPaymentIndex <- which(bond$couponDates > settleDate[i])[1]
    coupon <- bond$couponAmounts[nextPaymentIndex]
    accrued[i] <- coupon * getCouponTime.FIBond(bond, settleDate[i])

    if (bond$formula == "OFZ") {
      accrued[i] <-
        round(accrued[i] / bond$initialFace * 1000, digits = 2) *
        bond$initialFace / 1000
    }

    } else {

      accrued[i] <- NA

    }

  }

  return (accrued)

}


#' Calculate accrued interest in percentage of current face
#'
#' @param bond FIBond object
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Accrued interest in percentage of current face
#' @export
getAccrued.FIBond <- function(bond, settleDate = nextBizDay()) {
    getAccruedValue.FIBond(bond, settleDate) / getCurrentFace.FIBond(bond, settleDate) * 100
  }


#' Calculate value of FIBond object
#'
#' @param bond FIBond object
#' @param yield FIBond yield in percentage (can be a vector)
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Dirty value of bond object
#' @export
getValue.FIBond <- function(bond, yield, settleDate = nextBizDay()) {

  len <- hlpr::checkParams(yield, settleDate)

  value <- numeric()

  for (i in 1:len) {

    if (e(settleDate, i) >= bond$issueDate && e(settleDate, i) <= bond$maturity) {

      spans <- as.numeric(bond$couponDates - e(settleDate, i))

      if (bond$formula == "OFZ") {

        factors <- (spans > 0) * 1 / (1 + e(yield, i) / 100) ^ (spans / 365)
        payments <- bond$couponAmounts + bond$faceAmounts

      } else {

        numOfFutureCoupons <- length(spans[spans > 0])

        factors <-
          1 / (1 + e(yield, i) / 100 / bond$couponFreq) ^
          (1:numOfFutureCoupons - getCouponTime.FIBond(bond, e(settleDate, i)))

        payments <-
          tail(bond$couponAmounts + bond$faceAmounts,
               numOfFutureCoupons)

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
#' @param bond FIBond object
#' @param yield FIBond yield in percentage (can be a vector)
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Clean price of FIBond object in percentage
#' @export
getPrice.FIBond <- function(bond, yield, settleDate = nextBizDay()) {

    price <-
      (getValue.FIBond(bond, yield, settleDate) - getAccruedValue.FIBond(bond, settleDate)) /
      getCurrentFace.FIBond(bond, settleDate) * 100

    return (price)

  }


#' Calculate yield of FIBond object
#'
#' @param bond FIBond object
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

  len <- hlpr::checkParams(price, settleDate)

  for (i in 1:len) {

    if (e(settleDate, i) >= bond$issueDate && e(settleDate, i) <= bond$maturity) {

      f <- function(x) (getPrice.FIBond(bond, x, e(settleDate, i)) - e(price, i))
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
#' @param bond FIBond object
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
#' @param bond FIBond object
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

  len <- hlpr::checkParams(price, settleDate1, settleDate2, repoRate)

  carry <- numeric()

  for (i in 1:len) {

    inPlay <- which(bond$couponDates > e(settleDate1, i) & bond$couponDates <= e(settleDate2, i))

    payments <- bond$couponAmounts[inPlay] + bond$faceAmounts[inPlay]

    couponDates <- bond$couponDates[inPlay]

    carry[i] <-
      getAccruedValue.FIBond(bond, e(settleDate2, i)) - getAccruedValue.FIBond(bond, e(settleDate1, i)) +
      sum(payments * (1 + e(repoRate, i) / 100 * as.numeric(e(settleDate2, i) - couponDates) / 360)) -
      (
        e(price, i) / 100 * getCurrentFace.FIBond(bond, e(settleDate1, i)) +
          getAccruedValue.FIBond(bond, e(settleDate1, i))
      ) *
      e(repoRate, i) / 100 * as.numeric(e(settleDate2, i) - e(settleDate1, i)) / 360

  }

  return (carry)

}


#' Calculate carry for FIBond object in percentage of current face
#'
#' @param bond FIBond object
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
    getCurrentFace.FIBond(bond, settleDate1) * 100

}
