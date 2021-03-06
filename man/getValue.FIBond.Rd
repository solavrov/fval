% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FIBond.R
\name{getValue.FIBond}
\alias{getValue.FIBond}
\title{Calculate value of FIBond object}
\usage{
getValue.FIBond(bond, yield, settleDate = nextBizDay())
}
\arguments{
\item{bond}{FIBond object (can be a list)}

\item{yield}{FIBond yield in percentage (can be a vector)}

\item{settleDate}{Calculation date (can be a vector)}
}
\value{
Dirty value of bond object
}
\description{
Calculate value of FIBond object
}
