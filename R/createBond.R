createBond<-function(cash.flows,face = 100,issue.date = NULL,isin = "0",...)
{
  print(is.null(issue.date))
  if(is.null(issue.date))
  {
    tmp = index(cash.flows)[1:2]
    diff.tmp = diff(tmp)
    issue.date = tmp[1]-diff.tmp
    #Should work             print(issue.date)
  }
  return(new("bond",cash.flows,face,issue.date,isin))
}


createBondSyn<- function(face = 100, maturity = 1, coupon = 0.1, n.pay = 2,isin = "0") 
  {
    N = maturity
    maturity = maturity*365
    cf.time  = seq(0,maturity,length.out = (n.pay*N+1))
    issue.date = as.Date(cf.time)[1]
    cf.time  = as.Date(cf.time)[-1]
    cf       = rep(face*coupon/n.pay, length(cf.time))
    cf[length(cf)] = cf[length(cf)]+face
    cf       = matrix(cf)
    cf       = xts(cf, cf.time)
    
    return( createBond(cf, face, issue.date, isin) )
}
    



#createBondSyn()
