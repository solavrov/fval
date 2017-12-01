
#' @export
FIBONDS_FOLDER <- "fval_data/fibonds/"

#' @export
TFUTURES_FOLDER <- "fval_data/tfutures/"

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
    stop("ERROR! ", file, " is not found")
  }

  return (m)

}


