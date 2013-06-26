getKeyRateConvexity<-function(r,bond,id = NULL)
{
  CFd = getCFdisc(r,bond)
  t = getCFTime(bond)
  price = getBondPrice(bond,r)
  
  krd = CFd*t*t/price
  
  if(!is.null(id))
  {
    n = length(id)
    result = 1:n
    
    for(i in 1:n)
    {
      result[i] = krd[which(round(id[i],4)==round(t,4))]
    }
  }
  else result = krd
  
  result
}