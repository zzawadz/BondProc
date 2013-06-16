# M-absolutne

getMabs<-function(r,bond,H,price = NULL)
{
  if(is.null(price)) price = bondPrice(r,bond)
  t = getCFTime(bond)
  CFd = getCFdisc(r,bond)
  tH = abs(t-H)
  sum(CFd*tH)/price
}
