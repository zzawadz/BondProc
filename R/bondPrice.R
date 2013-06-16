bondPrice<-function(r,bond)
{
  sum(getCFdisc(r,bond))
}

