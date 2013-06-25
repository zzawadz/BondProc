


x =  read.csv2("Parser/kupony.csv",head = T, stringsAsFactors = F)

col.names = x[1,]
x = x[-1,]

colnames(x) <- col.names


ISIN = x[,"Kod ISIN"]

c.start = which(substr(col.names,1,4)=="Koni")

p_date = NULL
p_cf = NULL
p_isin = NULL
p_start = NULL
p_end = NULL
p_end2 = NULL
p_coupon = NULL
p_start2 = NULL

k = 0

for(j in 1:nrow(x))
{
  if(x[j,1]=="") next;
  k = k+1
  p_start = c(p_start,x[j,which(substr(col.names,1,4)=="Pocz")[1]])
  p_end = c(p_start,x[j,which(substr(col.names,1,11)=="Data wykupu")[1]])
  p_coupon = c(p_coupon,x[j,which(substr(col.names,1,5)=="Kupon")[1]])
  
  z = 0
  for(i in c.start)
  {
    if(x[j,i]=="" | x[j,i]=="-") break;
    z = z+1
    p_isin = c(p_isin, ISIN[j])
    p_date = c(p_date, x[j,i])
    p_cf = c(p_cf, as.numeric(gsub(",",".",x[j,i+3])))
  }
  p_end2 = c(p_end2,rep(p_end[length(p_end)],z))
  p_start2 = c(p_start2,rep(p_start[length(p_start)],z))
}

p_coupon = gsub(",",".",p_coupon)
p_coupon = as.numeric(gsub("%","",p_coupon))/100

save(p_date,p_cf,p_isin,p_start,p_start2,p_end,p_end2,p_coupon,file = "../BondProc/data/bonddata.rda")
