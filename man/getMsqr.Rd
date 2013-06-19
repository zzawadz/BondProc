\name{getMsqr}
\alias{getMsqr}
\title{Order a data frame by its columns.}
\usage{
  getMsqr(r, bond, H, price = NULL)
}
\arguments{
  \item{r}{\link{yield curve}}

  \item{bond}{object of class \link{bond} for which M^2 is
  to be calculated.}

  \item{H}{period for M^2.}

  \item{price}{for bond, if NULL \link{getBondPrice} will
  be used.}
}
\description{
  This function return M^2 for specified period
}
\keyword{M^2}

