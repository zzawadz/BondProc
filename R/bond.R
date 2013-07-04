setOldClass("xts")
setClass(Class="bond",representation=list(
  face = "numeric",
  issue.date = "Date",
  price = "numeric",
  cash.flows = "xts",
  isin = "character"
 ),
          prototype = list(
            face = 100,
            issue.date = as.Date("2011-01-01"),
            price = 0,
            cash.flows = xts(),
            isin = "0")
          
)



setMethod("initialize","bond",
          function(.Object, cash.flows, face, issue.date, isin)
          {
            .Object@face = face
            .Object@issue.date = issue.date 
            .Object@cash.flows = cash.flows
            .Object@price = numeric()
            .Object@isin = as.character(isin)
            .Object
          }
)



setGeneric("getIssueDate", function(bond) {
  standardGeneric("getIssueDate")
})

setMethod("getIssueDate", "bond",
          function(bond)
          {
            return(bond@issue.date)
          }
)

setGeneric("getISIN", function(bond) {
  standardGeneric("getISIN")
})

setMethod("getISIN", "bond",
          function(bond)
          {
            return(bond@isin)
          }
)







#Zwraca zdyskontowane przeplywy pieniezne
#Kapitalizacja zlozona

setGeneric("getCFdisc", function(r,bond) {
  standardGeneric("getCFdisc")
})

setMethod("getCFdisc", "numeric",
          function(r,bond)
          {
            CF = getCF(bond)
            t  = getCFTime(bond)
            CFd = CF*exp(-r*t)
            return(CFd)
          }
)

setMethod("getCFdisc", "NelsonSiegel",
          function(r,bond)
          {
            t = getCFTime(bond)
            r = getNelsonSiegelIntrestRates(r,t)
            CFd = getCFdisc(r,bond)
            return(CFd)
          }
)

setMethod("getCFdisc", "function",
          function(r,bond)
          {
            t = getCFTime(bond)
            r = r(t)
            CFd = getCFdisc(r,bond)
            return(CFd)
          }
)

getBondPrice<-function(bond,r=NULL)
{
  if(length(bond@price)==0)
  {
    if(is.null(r)) stop("Nie moge obliczyc ceny - nie podano struktury terminowej")
    return(bondPrice(r,bond))
  }
  else return(bond@price)
}

# Test
# x = newBond(100,1,0.1,1,r=0.5)
#getCF(x)

#x = newBond(face=100,maturity=10,coupon=0.1,n.pay=4)
#f<-function(x) x/100
