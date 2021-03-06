% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FIBond.R
\name{getCouponTime.FIBond}
\alias{getCouponTime.FIBond}
\title{Calculate coupon time i.e. days passed over days in coupon period for FIBond object}
\usage{
getCouponTime.FIBond(bond, settleDate = nextBizDay())
}
\arguments{
\item{bond}{FIBond object (can be a list)}

\item{settleDate}{Calculation date (can be a vector)}
}
\value{
Coupon time for FIBond object
}
\description{
Calculate coupon time i.e. days passed over days in coupon period for FIBond object
}
