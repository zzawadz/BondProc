#Przy pierwszym uruchomieniu nalezy wykonac kroki
# ktore sa zakomentowane - instalacja wymaganych pakietow itp

#instalacja pakietu pozwalajaca zainstalowac paczke z GitHuba
#install.packages("devtools)

#instalacja pakietu bezposrednio z githuba

#devtools::install_github("BondProc", "zzawadz")


#https://github.com/zzawadz/BondProc - adres repozytorium
#mozna tam podejrzec kod

#Wymagany pakiet do optymalizacji 
#nie instaluje sie sam!!!!
#install.packages("lpSolve")


require(BondProc)
# Przyklad immunizacji portfela obligacji przy pomocy M^2

# Trzy przykładowe obligacje:
bond1 = newBond(face=100,maturity=12,coupon=0.1,n.pay=1)
bond2 = newBond(face=100,maturity=9,coupon=0.08,n.pay=1)
bond3 = newBond(face=100,maturity=5,coupon=0.07,n.pay=1)

# Stworzenie koszyka obligacji, który pozniej wrzucany jest do funkcji
# wyznaczajacej optymalny portfel, koszyk tworzony jest poprzez "dodanie" do siebie obligacji 
basket = bond1 + bond2 + bond3


#Struktura stóp terminowych podana jako funckja czasu
rf = function(t) (0.5*t+2.5)/100

portfolio = immunizationMsqr(basket=basket, r=rf, H = 7)
print(portfolio)

immCF = newBond(face=10000,maturity=7,coupon=0.00,n.pay=1)

#Ilosc obligacji:
getBondPrice(immCF,rf)*portfolio$weights/getBondPrice(basket,rf)

#Maks zmiana procentowa:
deltaF = 0.04
portfolio$portfolio.Msqr/2*deltaF
  
  
