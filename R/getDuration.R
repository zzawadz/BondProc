getDuration<-function(r,bond,method = "FW")
{
  D = sum(getCFdisc(r,bond)*getCFTime(bond))
  return(D/getBondPrice(bond,r))
}
