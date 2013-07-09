\name{createBondSyn}
\alias{createBondSyn}
\title{Create synthetic bond.}
\usage{
  createBondSyn(face = 100, maturity = 1, coupon = 0.1,
    n.pay = 1, isin = "0")
}
\arguments{
  \item{face}{face value}

  \item{maturity}{time to maturity (in years)}

  \item{coupon}{coupon}

  \item{n.pay}{number of coupons per year}

  \item{isin}{bond identification number}
}
\description{
  This function return bond object created from synthetic
  data. It is for educational purposes.
}
\examples{
bond = createBondSyn()
getBondPrice(bond,0.1,theoretical=T)
}
\keyword{M^2}

