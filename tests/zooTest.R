require(xts)

?xts

data(sample_matrix)
sample.xts <- as.xts(sample_matrix, descr='my new xts object')

class(sample.xts)

sample_matrix


x = xts(x=1:100,as.Date(1:100))
ndays(x[1:2])

x = as.Date("2000-03-03")
x2 = x+365

newBond(face=100,maturity=x2,issue.date=x,coupon=0.1,n.pay=1)