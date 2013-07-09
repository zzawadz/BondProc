\name{createBond}
\alias{createBond}
\title{Order a data frame by its columns.}
\usage{
  createBond(cash.flows, face = 100, issue.date = NULL,
    isin = "0", ...)
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
  This function return M^4 for specified period
}
\keyword{M^2}

