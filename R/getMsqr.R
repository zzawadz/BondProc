# M-kwadrat

getMsqr <-function(r,bond,H,price = NULL)
{
  if(is.null(price)) price = bondPrice(r,bond)
  t = getCFTime(bond)
  CFd = getCFdisc(r,bond)
  tH = (t-H)^2
  sum(CFd*tH)/price
}