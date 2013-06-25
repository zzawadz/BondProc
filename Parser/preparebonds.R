
getAvaISIN<-function()
{
  unique(p_isin)
}

getBondStructure <- function(isin,price = 100,today, country = "Poland")
{
  data(bonddata)
  ISIN <- vector()
  MATURITYDATE <- vector()
  STARTDATE <- vector()
  COUPONRATE <- vector()
  PRICE <- vector()
  ACCRUED <- vector()
  CFISIN <- vector()
  CF <- vector()
  DATE <- vector()
  
  TODAY <- vector()

  for(i in 1:length(isin))
  {
    k = which(p_isin==isin[i] & as.Date(p_date)>as.Date(today))
    p = which(unique(p_isin)==isin[i])
    
    ISIN         = c(ISIN, unique(p_isin[k]))
    CFISIN       = c(CFISIN, p_isin[k])
    CF           = c(CF, p_cf[k])
    DATE         = c(DATE, p_date[k])
    
    STARTDATE    = c(STARTDATE,p_start[p])
    MATURITYDATE = c(MATURITYDATE, p_end[p])
    COUPONRATE   = c(COUPONRATE, p_coupon[p])
    ACCRUED      = c(ACCRUED,1)
  }
  
  if(length(price)==1) PRICE = rep(price,length(ISIN))
  else PRICE = price
  
  TODAY = as.Date(today)
  STARTDATE = as.Date(STARTDATE)
  DATE  = as.Date(DATE)
  MATURITYDATE = as.Date(MATURITYDATE)
  
  CASHFLOWS <- list(CFISIN,CF,DATE)
  names(CASHFLOWS) <- c("ISIN","CF","DATE")

  mycountry1 <- list(ISIN,MATURITYDATE,STARTDATE,
                     COUPONRATE,PRICE,ACCRUED,CASHFLOWS,TODAY)
  
  names(mycountry1) <- c("ISIN","MATURITYDATE","STARTDATE","COUPONRATE",
                         "PRICE","ACCRUED","CASHFLOWS","TODAY")
  mybonds <- list(mycountry1)
  names(mybonds) <- c("Poland")
  class(mybonds)="couponbonds"
  return(mybonds)  
}

#pol.str = getBondStructure(getAvaISIN(),100,"2011-03-03")

#ns_res <- estim_nss(pol.str, c("Poland"),matrange = c(0,30), method = "ns", tauconstr = list(c(0.2,5,0.1),c(0.2,5,0.1), c(0.2,5,0.1)))