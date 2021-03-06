% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EurOption.R
\name{EurOption}
\alias{EurOption}
\title{Constructor of european option object}
\usage{
EurOption()
}
\value{
EurOption object
}
\description{
Attributes:
$type - call or put
$strike - strike
$time - time to expiration in years
$rate - risk free rate
$yield - dividend yield of underlying stock or else
$vol - volatility of underlying
}
