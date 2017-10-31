
#' Load historical market prices and rates
#'
#' @param file File name of csv data file without extension
#'
#' @return Data frame of market data
#' @export
loadMarket <- function(file = "market") {
  read.csv(paste0("fval_data/", file, ".csv"))
}
