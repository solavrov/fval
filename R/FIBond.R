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

  list <- list()

  for (i in 1:length(file)) {

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

    if (!is.na(file[i])) {

      file[i] <- paste0("fval_data/", file[i], ".csv")

      if (file.exists(file[i])) {

        df <- read.csv(file[i], sep = sep)

        if (!is.null(df$name)) bond$name <- as.character(df$name[1])
        if (!is.null(df$isin)) bond$isin <- as.character(df$isin[1])
        if (!is.null(df$currency)) bond$currency <- as.character(df$currency[1])

        if (!is.null(df$issueDate)) bond$issueDate <-
            as.Date(lubridate::parse_date_time(as.character(df$issueDate[1]), dateFormat))

        if (!is.null(df$formula)) bond$formula <- as.character(df$formula[1])
        if (!is.null(df$dayCounter)) bond$dayCounter <- df$dayCounter[1]
        if (!is.null(df$cfactor)) bond$cfactor <- df$cfactor[1]

        if (!is.null(df$couponDates)) {
          bond$couponDates <-
            as.Date(lubridate::parse_date_time(as.character(df$couponDates), dateFormat))
          bond$maturity <- tail(bond$couponDates, 1)
          bond$couponFreq <-
            round(length(bond$couponDates) / as.numeric(bond$maturity - bond$issueDate) * 365)
        }

        if (!is.null(df$couponAmounts)) bond$couponAmounts <- df$couponAmounts

        if (!is.null(df$faceAmounts)) {
          bond$faceAmounts <- df$faceAmounts
          bond$initialFace <- sum(df$faceAmounts)
        }

      }  else {

        cat("ERROR!", file[i], "is not found\n")

      }

    }

    list[[i]] <- bond
    if (!is.na(bond$name)) names(list)[i] <- bond$name

  }

  if (length(list) == 1)
    return (list[[1]])
  else
    return (list)

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


#' Return cfactor of FIBond object
#'
#' @param bond FIBond object (can be a list)
#'
#' @return CFactor (can be a vector)
#' @export
takeCFactor.FIBond <- function(bond) {

  len <- L(bond)

  cf <- numeric()

  for (i in 1:len) {
    cf[i] <- E(bond, i)$cfactor
  }

  return (cf)

}

#' Calculate coupon time i.e. days passed over days in coupon period for FIBond object
#'
#' @param bond FIBond object (can be a list)
#' @param settleDate Calculation date (can be a vector)
#'
#' @return Coupon time for FIBond object
#' @export
getCouponTime.FIBond <- function(bond, settleDate = nextBizDay()) {

  len <- L(bond, settleDate)

  time <- numeric()

  for (i in 1:len) {

    bond.i <- E(bond, i)
    settleDate.i <- E(settleDate, i)

    if (settleDate.i >= bond.i$issueDate && settleDate.i <= bond.i$maturity) {

      nextPaymentIndex <- which(bond.i$couponDates > settleDate.i)[1]
      nextDay <- bond.i$couponDates[nextPaymentIndex]

      if (nextPaymentIndex >= 2) {
        prevDay <- bond.i$couponDates[nextPaymentIndex - 1]
      } else {
        prevDay <- bond.i$issueDate
      }

      period <- RQuantLib::dayCount(prevDay, nextDay, bond.i$dayCounter)
      daysPassed <- RQuantLib::dayCount(prevDay, settleDate.i, bond.i$dayCounter)

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

  len <- L(bond, settleDate)

  face <- numeric()

  for (i in 1:len) {

    bond.i <- E(bond, i)
    settleDate.i <- E(settleDate, i)

    if (settleDate.i >= bond.i$issueDate && settleDate.i <= bond.i$maturity) {

      face[i] <- sum(bond.i$faceAmounts[bond.i$couponDates > settleDate.i])

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

  len <- L(bond, settleDate)

  accrued <- numeric()

  for (i in 1:len) {

    bond.i <- E(bond, i)
    settleDate.i <- E(settleDate, i)

    if (settleDate.i >= bond.i$issueDate && settleDate.i <= bond.i$maturity) {

      nextPaymentIndex <- which(bond.i$couponDates > settleDate.i)[1]
      coupon <- bond.i$couponAmounts[nextPaymentIndex]
      accrued[i] <- coupon * getCouponTime.FIBond(bond.i, settleDate.i)

      if (bond.i$formula == "OFZ") {
        accrued[i] <-
          round(accrued[i] / bond.i$initialFace * 1000, digits = 2) * bond.i$initialFace / 1000
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

  len <- L(bond, yield, settleDate)

  value <- numeric()

  for (i in 1:len) {

    bond.i <- E(bond, i)
    yield.i <- E(yield, i)
    settleDate.i <- E(settleDate, i)

    if (settleDate.i >= bond.i$issueDate && settleDate.i <= bond.i$maturity) {

      spans <- as.numeric(bond.i$couponDates - settleDate.i)

      if (bond.i$formula == "OFZ") {

        factors <- (spans > 0) * 1 / (1 + yield.i / 100) ^ (spans / 365)
        payments <- bond.i$couponAmounts + bond.i$faceAmounts

      } else {

        numOfFutureCoupons <- length(spans[spans > 0])

        factors <- 1 / (1 + yield.i / 100 / bond.i$couponFreq) ^
          (1:numOfFutureCoupons - getCouponTime.FIBond(bond.i, settleDate.i))

        payments <- tail(bond.i$couponAmounts + bond.i$faceAmounts, numOfFutureCoupons)

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

  len <- L(bond, price, settleDate)

  for (i in 1:len) {

    bond.i <- E(bond, i)
    price.i <- E(price, i)
    settleDate.i <- E(settleDate, i)

    if (settleDate.i >= bond.i$issueDate && settleDate.i <= bond.i$maturity) {

      f <- function(x) (getPrice.FIBond(bond.i, x, settleDate.i) - price.i)
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

  len <- L(bond, price, settleDate1, settleDate2, repoRate)

  carry <- numeric()

  for (i in 1:len) {

    bond.i <- E(bond, i)
    price.i <- E(price, i)
    settleDate1.i <- E(settleDate1, i)
    settleDate2.i <- E(settleDate2, i)
    repoRate.i <- E(repoRate, i)

    inPlay <- which(bond.i$couponDates > settleDate1.i & bond.i$couponDates <= settleDate2.i)

    payments <- bond.i$couponAmounts[inPlay] + bond.i$faceAmounts[inPlay]

    couponDates <- bond.i$couponDates[inPlay]

    carry[i] <-
      getAccruedValue.FIBond(bond.i, settleDate2.i) - getAccruedValue.FIBond(bond.i, settleDate1.i) +
      sum(payments * (1 + repoRate.i / 100 * as.numeric(settleDate2.i - couponDates) / 360)) -
      (
        price.i / 100 * getFace.FIBond(bond.i, settleDate1.i) +
          getAccruedValue.FIBond(bond.i, settleDate1.i)
      ) *
      repoRate.i / 100 * as.numeric(settleDate2.i - settleDate1.i) / 360

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
