getKeyRateDuration<-function(r,bond,id = NULL)
{
  CFd = getCFdisc(r,bond)
  t = getCFTime(bond)
  price = getBondPrice(bond,r)
  
  krd = CFd*t/price
  
  if(!is.null(id))
  {
    n = length(id)
    result = 1:n
    
    for(i in 1:n)
    {
      result[i] = krd[which(id[i]==t)]
    }
  }
  else result = krd
  
  result
}

# rf = function(t) (4.5+2*t)/100
# bond = newBond(100,maturity=2,coupon=0.1,n.pay=4)
# getKeyRateDuration(rf,bond)