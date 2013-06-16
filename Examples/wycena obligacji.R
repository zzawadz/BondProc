# Wycena obligacji

########################################
# Do wyceny sluzy funkcja getBondPrice #
########################################
# Stala stopa procentowa

#Przykladowa obligacja
bond = newBond(face = 100, maturity= 10, coupon= 1:10/100, n.pay= 1)

getBondPrice(bond,r=0.1)

# Stopa procentowa wyznaczana z modelu Nelsona-Siegela
# Zadanie 8 z powtorki do kolokwium 1
# Zmienna stopa kuponowa
bond = newBond(face = 100, maturity= 10, coupon= 1:10/100, n.pay= 1)

# Tworzenie obiektu z parametrami Nelsona-Siegela
ns = newNS(alpha1=0.01,alpha2= 0.02, alpha3= 0,beta = 1)

#Obliczenie ceny
getBondPrice(bond,ns)

# Stopa procentowa podana jako funckcja czasu:

rt <-function(t) t/100
getBondPrice(bond,rt)