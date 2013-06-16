getVectorDuration<-function(r,bond,m)
{
  CFd = getCFdisc(r,bond)
  t = getCFTime(bond)
  price = getBondPrice(bond,r)
  
  result = 1:length(m)
  
  for(i in 1:length(m))
    result[i] = sum(CFd*t^i)/price
  
  return(result)
}

getVectorDurationMatrix <- function(r,basket,m)
{
  n = length(basket)
  
  result = NULL
  
  for(i in 1:n)
  {
    result = rbind(result,getVectorDuration(r,basket[[i]],1:m))
  }
  
  return(result)
}