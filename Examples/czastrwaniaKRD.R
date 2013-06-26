basket = newBondBasket()

for(i in 1:10)
{
  basket = basket + newBond(face = 100,maturity=i,coupon=0.05,n.pay=1)
}

ns = newNelsonSiegel(alpha1=0.05,alpha2=0.04,0.1,10)


getKeyRateDuration(ns,bond=basket[[1]],id=1)
getKeyRateDuration(ns,bond=basket[[2]],id=1:2)

X = matrix(0,ncol = 10,nrow = 10)

for(i in 1:length(basket)) X[1:i,i]= getKeyRateDuration(ns,bond=basket[[i]],id=1:i)
X = X[-1,]
X = rbind(X,1)

b = rep(0,10)
b[10] = 1
b[6]  = 7


solve(X)%*%b