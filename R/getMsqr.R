#' Order a data frame by its columns.
#'
#' This function return M^2 for specified period
#'
#' @param r \link{yield curve}
#' @param bond object of class \link{bond} for which M^2 is to be calculated.
#' @param H period for M^2.
#' @param price for bond, if NULL \link{getBondPrice} will be used.
#' @keywords M^2
#' @export



getMsqr <-function(r,bond,H,price = NULL)
{
  if(is.null(price)) price = bondPrice(r,bond)
  t = getCFTime(bond)
  CFd = getCFdisc(r,bond)
  tH = (t-H)^2
  sum(CFd*tH)/price
}