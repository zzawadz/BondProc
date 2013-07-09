# NEW VERSION

setOldClass("xts")
setClass(Class="bond",representation=list(
  face = "numeric",
  issue.date = "POSIXct",
  price = "numeric",
  cash.flows = "xts",
  isin = "character"
 ),
          prototype = list(
            face = 100,
            issue.date = as.POSIXct("2011-01-01"),
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
