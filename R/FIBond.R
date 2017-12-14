
#' @export
FOLDER_FIBONDS <- "fval_data/FIBonds/"

#' @export
FOLDER_FIBONDS_US_GOV <- "fval_data/FIBonds/US_GOV/"

#' @export
FOLDER_FIBONDS_RU_GOV <- "fval_data/FIBonds/RU_GOV/"

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
#' @param file Name, without extension, of csv file that contains bond attributes.
#' Heads should have names of attributes.
#' @param dateFormat File date format from lubridate package i.e. "dmy", "mdy" etc
#' @param sep Separator i.e. comma, semicolon or else
#'
#' @return FIBond object
#' @export
FIBond <- function(file = NA, folder = FOLDER_FIBONDS_US_GOV, dateFormat = "mdy", sep = ",") {

  toBuild <- TRUE

  if (is.na(file))
    df <- data.frame()
  else if (file.exists(path <- paste0(folder, file, ".csv")))
    df <- read.csv(path, sep = sep)
  else {
    toBuild <- FALSE
    warning("FIBond's file does not exist")
  }

  if (toBuild) {

    bond <- list()
    class(bond) <- "FIBond"

    get <- function(df, name, asChar = FALSE, ifNull = NA) {

      result <- df[[name]]

      if(is.null(result))
        result <- ifNull
      else if (asChar)
        result <- as.character(result)

      return (result)

    }

    bond$name <- get(df, "name", asChar = TRUE)[1]
    bond$risk <- get(df, "risk", asChar = TRUE)[1]
    bond$isin <- get(df, "isin", asChar = TRUE)[1]
    bond$currency <- get(df, "currency", asChar = TRUE)[1]
    bond$issueDate <- parseDate(get(df,"issueDate")[1], dateFormat)
    bond$formula <- get(df, "formula", asChar = TRUE, ifNull = "STD")[1]
    bond$dayCounter <- get(df, "dayCounter", ifNull = DAY_COUNTER$ActualActual)[1]
    bond$cfactor <- get(df, "cfactor")[1]
    bond$couponDates <- parseDate(get(df,"couponDates"), dateFormat)
    bond$maturity <- tail(bond$couponDates, 1)
    bond$couponFreq <- round(length(bond$couponDates) /
                               as.numeric(bond$maturity - bond$issueDate) * 365)
    bond$couponAmounts <- get(df, "couponAmounts")
    bond$faceAmounts <- get(df, "faceAmounts")
    bond$initialFace <- sum(bond$faceAmounts)
    bond$initialCoupon <- bond$couponAmounts[1]

  } else {

    bond <- NA

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

  cat("name:          ", bond$name, "\n")
  cat("risk:          ", bond$risk, "\n")
  cat("isin:          ", bond$isin, "\n")
  cat("currency:      ", bond$currency, "\n")
  cat("initialFace:   ", bond$initialFace, "\n")
  cat("initialCoupon: ", bond$initialCoupon, "\n")
  cat("couponFreq:    ", bond$couponFreq, "\n")
  cat("issueDate:     ", as.character(bond$issueDate), "\n")
  cat("maturity:      ", as.character(bond$maturity), "\n")
  cat("formula:       ", bond$formula, "\n")
  cat("dayCounter:    ", counterName(bond$dayCounter), "\n")
  cat("cfactor:       ", bond$cfactor, "\n\n")
  print(
    data.frame(
      couponDates = bond$couponDates,
      couponAmounts = bond$couponAmounts,
      faceAmounts = bond$faceAmounts
    )
  )

}


#' Show attributes of all bonds from a given folder
#'
#' @param folder Folder
#' @param attr Vector of attributes' names
#'
#' @return Data frame with bonds' attributes
#' @export
dir.FIBond <- function(folder = FOLDER_FIBONDS_US_GOV,
                       attr = c("name", "isin", "risk", "issueDate", "maturity")) {

  files <- list.files(folder)
  files <- substr(files, 1, nchar(files) - 4)

  l <- vector("list", length(attr))
  names(l) <- attr

  for (i in 1:length(files)) {
    b <- FIBond(files[i], folder = folder)
    for (j in 1:length(attr)) {
      if (i == 1)
        l[[j]] <- b[[attr[j]]]
      else
        l[[j]][i] <- b[[attr[j]]]
    }
  }

  df <- data.frame(l)
  names(df) <- attr
  df <- data.frame(list(fileName = files), df)

  return (df)

}


#' Calculate coupon time i.e. days passed over days in coupon period for FIBond object
#'
#' @param bond FIBond object
#' @param settleDate Calculation date
#'
#' @return List of $period (days), $passed (days), $time (ratio) values
#' @export
getTime.FIBond <- function(bond, settleDate = nextBizDay()) {

  time <- list()

  if (settleDate >= bond$issueDate && settleDate <= bond$maturity) {

    nextPaymentIndex <- which(bond$couponDates > settleDate)[1]
    nextDay <- bond$couponDates[nextPaymentIndex]

    if (nextPaymentIndex >= 2) {
      prevDay <- bond$couponDates[nextPaymentIndex - 1]
    } else {
      prevDay <- bond$issueDate
    }

    time$period <- countDays(prevDay, nextDay, bond$dayCounter)
    time$passed <- countDays(prevDay, settleDate, bond$dayCounter)
    time$time <- time$passed / time$period

  } else {

    time$period <- NA
    time$passed <- NA
    time$time <- NA

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


#' Return current coupon amount
#'
#' @param bond FIBond object
#' @param settleDate Calculation date
#'
#' @return Current coupon amount
#' @export
getCoupon.FIBond <- function(bond, settleDate = nextBizDay()) {

  if (settleDate >= bond$issueDate && settleDate <= bond$maturity) {
    nextPaymentIndex <- which(bond$couponDates > settleDate)[1]
    coupon <- bond$couponAmounts[nextPaymentIndex]
  } else {
    coupon <- NA
  }

  return (coupon)

}


# getCouponRate.FIBond <- function(bond, settleDate = nextBizDay()) {
#
#
#   if (bond$formula == "OFZ") {
#
#     rate <-
#
#   } else {
#     rate <- getCoupon.FIBond(bond, settleDate) * bond$couponFreq /
#       getFace.FIBond(bond, settleDate) * 100
#   }
#
#
#
# }


#' Calculate accrued interest for FIBond object
#'
#' @param bond FIBond object
#' @param settleDate Calculation date
#'
#' @return Accrued interest
#' @export
getAccruedValue.FIBond <- function(bond, settleDate = nextBizDay()) {

  accrued <-
    getCoupon.FIBond(bond, settleDate) * getTime.FIBond(bond, settleDate)$time

  if (bond$formula == "OFZ") {
    accrued <-
      round(accrued / bond$initialFace * 1000, digits = 2) * bond$initialFace / 1000
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
        (1:numOfFutureCoupons - getTime.FIBond(bond, settleDate)$time)

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


#' Return modified duration of given FIBond
#'
#' @param bond FIBond object
#' @param price FIBond clean price in percentage
#' @param settleDate Calculation date
#'
#' @return Modified duration of given FIBond
#' @export
getDur.FIBond <- function(bond, price, settleDate = nextBizDay()) {
  - getPVBP.FIBond(bond, price, settleDate) * 100 /
    (price + getAccrued.FIBond(bond, settleDate)) * 100
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
