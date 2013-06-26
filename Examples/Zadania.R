#Zadanie 1
rf<-function(t) (0.5*t+2.5)/100


bond1 = newBond(face=100,maturity=12,coupon=0.1,n.pay=1)
bond2 = newBond(face=100,maturity=9,coupon=0.08,n.pay=1)
bond3 = newBond(face=100,maturity=5,coupon=0.07,n.pay=1)
bond4 = newBond(face=10000,maturity=7,coupon=0.00,n.pay=1)

getMabs(r=rf,bond=bond1,H=7)
getMabs(r=rf,bond=bond2,H=7)
getMabs(r=rf,bond=bond3,H=7)

# Szukana cena:
getBondPrice(bond4,r=rf)/getBondPrice(bond2,rf)

#Odchylenie ()
getMabs(r=rf,bond=bond2,H=7)*max(abs(c(0.02,0.03)))

######################
# Zadanie 2
#plik immunizacja Msqr

######################
#Zadanie 3
bond1 = newBond(face=100,maturity=5,coupon=0.05,n.pay=4)

r1 = 0.053; r2 = 0.069
t1 = 1; t2 = 5;

a = (r2-r1)/(t2-t1)
b = r1-a*t1 

rf<-function(t) a*t+b
getVectorDuration(r=rf,bond=bond1,1:5)

######################
# Zadanie 4 immunizacja wektor czasow trwania

rf<-function(t) (0.15*t+4.85)/100
bond1 = newBond(face=1000,maturity=1,coupon=0.3,n.pay=12)
getKeyRateDuration(rf,bond1,id=(1:12)/12)
getKeyRateConvexity(rf,bond1,id=(1:12)/12)

