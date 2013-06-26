# Zadanie 4 powtorzenie

basket = newBondBasket()

for(i in 1:10)
{
  basket = basket + newBond(face = 100,maturity=i,coupon=0.05,n.pay=1)
}

ns = newNelsonSiegel(alpha1=0.05,alpha2=0.04,0.1,10)


portfolio = immunizationDV(basket,ns,H=7,m=5)

immCF = newBond(face=10000,maturity=7,coupon=0.00,n.pay=1)
getBondPrice(immCF,ns)*portfolio/getBondPrice(basket,ns)