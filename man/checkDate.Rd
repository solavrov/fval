% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calendar.R
\name{checkDate}
\alias{checkDate}
\title{Return 1 if date within range and NA otherwise}
\usage{
checkDate(date, earliestDate = -Inf, latestDate = Inf)
}
\arguments{
\item{date}{Date (can be a vector)}

\item{earliestDate}{Earliest date, default value is minus infinity (-1e6)}

\item{latestDate}{Latest date, default value is plus infinity (1e6)}
}
\value{
1 if date within range and NA otherwise
}
\description{
Return 1 if date within range and NA otherwise
}
