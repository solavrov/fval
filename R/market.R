
#' Load historical market prices and rates
#'
#' @param file File name of csv data file without extension
#'
#' @return Data frame of market data
#' @export
loadMarket <- function(file = "market") {

  file <- paste0("fval_data/", file, ".csv")

  m <- NA

  if (file.exists(file)) {
    m <- read.csv(file)
    m$DATE <- as.Date(m$DATE)
  } else {
    cat("ERROR!", file, "is not found\n")
  }

  return (m)

}

