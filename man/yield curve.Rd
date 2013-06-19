\name{yield curve}
\alias{yield curve}
\title{
Yield curve in BondProc
}
\description{
This page describes usage of yield curve in BondProc packages.
}

\details{
Yield curve in Bond proc can be specyfied in four different ways:
 \itemize{
  \item{signle numeric}{ - this is considered as a flat yield curve}
  \item{multiple numeric}{ - this should have the same length as number of payments for specific bond, otherwise behavior of all functions is undefined. THIS OPTION IS NOT SUPPORTED BY \link{bondBasket}}
 
 \item{function of time}{ - yield curve can be passed as a function of time, see examples}
 \item{\link{NelsonSiegel}}{ - Nelson-Siegel model}
 }
}


\author{
Zygmunt Zawadzki
}

\examples{
bond = newBond(face=100,maturity=10,coupon=0.1,n.pay=6)

# Valuation of bonds

# Using sigle value
getBondPrice(bond,r= 0.1)

# Using multiple values
r = seq(1,10,length.out=60)/100
getBondPrice(bond,r)

# yield curve as a function
rf <- function(t) (t*2)/100
getBondPrice(bond,rf)

# Using Nelson-Siegel model
ns = newNelsonSiegel(alpha1=0.06,0.03,0.02,1)
getBondPrice(bond,ns)

}

