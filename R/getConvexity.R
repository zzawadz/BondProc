getConvexity<-function(r,bond,method = "FW")
{
  D = sum(getCFdisc(r,bond)*getCFTime(bond)^2)
  return(D/getBondPrice(bond,r))
}