
#' @export
MARKET_FOLDER <- "fval_data/market/"


#' Load historical market prices and rates
#'
#' @param file File name of csv data file without extension
#'
#' @return Data frame of market data
#' @export
loadMarket <- function(file = "market") {

  file <- paste0(MARKET_FOLDER, file, ".csv")

  m <- NA

  if (file.exists(file)) {
    m <- read.csv(file)
    m$DATE <- as.Date(m$DATE)
  } else {
    stop("File ", file, " is not found")
  }

  return (m)

}


#' Show prime attributes of all bonds from a given folder
#'
#' @param folder Folder
#'
#' @return Data frame with bonds' attributes
#' @export
showFolder.FIBond <- function(folder = FIBONDS_FOLDER) {

  name <- character()
  risk <- character()
  isin <- character()
  issueDate <- character()
  maturity <- character()

  files <- list.files(folder)
  files <- substr(files, 1, nchar(files) - 4)

  for (i in 1:length(files)) {
    b <- FIBond(files[i])
    name[i] <- b$name
    risk[i] <- b$risk
    isin[i] <- b$isin
    issueDate[i] <- as.character(b$issueDate)
    maturity[i] <- as.character(b$maturity)
  }

  issueDate <- as.Date(issueDate)
  maturity <- as.Date(maturity)

  df <- data.frame(name = name, risk = risk, isin = isin,
                   issueDate = issueDate, maturity = maturity)

  return (df)

}



