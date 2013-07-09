# NEW VERSION

#' Order a data frame by its columns.
#'
#' This function return M^4 for specified period
#'
#' @param r \link{yield curve}
#' @param bond object of class \link{bond} for which M^2 is to be calculated.
#' @param H period for M^2.
#' @param price for bond, if NULL \link{getBondPrice} will be used.
#' @keywords M^2
#' @export

createBond<-function(cash.flows,face = 100,issue.date = NULL,isin = "0",...)
{
  if(is.null(issue.date))
  {
    tmp = index(cash.flows)[1:2]
    diff.tmp = diff(tmp)
    issue.date = tmp[1]-diff.tmp
  }
  return(new("bond",cash.flows,face,issue.date,isin))
}

############################################################

#' Create synthetic bond.
#'
#' This function return bond object created from synthetic data. It is for educational purposes.
#'
#' @param face face value
#' @param maturity time to maturity (in years)
#' @param coupon coupon
#' @param n.pay number of coupons per year
#' @param isin bond identification number
#' @keywords M^2
#' @export
#' @examples
#' 
#' bond = createBondSyn()
#' getBondPrice(bond,0.1,theoretical=T)


createBondSyn<- function(face = 100, maturity = 1, coupon = 0.1, n.pay = 1,isin = "0") 
  {
    N = maturity
    maturity = maturity*365*3600*24
    cf.time  = as.POSIXct( seq(0,maturity,length.out = (n.pay*N+1)),origin="1970-01-01")
    issue.date = cf.time[1]
    cf.time  = cf.time[-1]
    cf       = rep(face*coupon/n.pay, length(cf.time))
    cf[length(cf)] = cf[length(cf)]+face
    cf       = matrix(cf)
    cf       = xts(cf, cf.time)
    
    return( createBond(cf, face, issue.date, isin) )
}
    



#createBondSyn()
