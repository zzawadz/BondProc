# OLD VERSION

getConvexity<-function(bond,r,today = NULL,method = "FW")
{
  D = sum(getCFdisc(r,bond)*getCFTime(bond)^2)
  return(D/getBondPrice(bond,r))
}