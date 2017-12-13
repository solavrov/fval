
#' Return present value of periodic payments
#'
#' @param rate Interest rate in percentage
#' @param nper Number of periods (can be not a whole number)
#' @param pmt Payment (all payments are equal)
#' @param fv Future value
#' @param lessAccrued TRUE for deducting accrued, FALSE otherwise
#'
#' @return Present value of periodic payments
#' @export
getPV.TVM <- function(rate, nper, pmt, fv, lessAccrued = FALSE) {

  if (rate == 0)
    pv <- ceiling(nper) * pmt + fv
  else
    pv <-
      (pmt * (1 * (nper %% 1 != 0) + 100 / rate) *
         (1 - 1 / (1 + rate / 100) ^ ceiling(nper)) +
         fv / (1 + rate / 100) ^ floor(nper)) /
        (1 + rate / 100) ^ (nper - floor(nper))

  if (lessAccrued)
    return (pv - pmt * (ceiling(nper) - nper))
  else
    return (pv)

}

