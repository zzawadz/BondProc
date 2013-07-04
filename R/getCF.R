setGeneric("getCF", function(bond, today = NULL) {
  standardGeneric("getCF")
})
setMethod("getCF", "bond",
          function(bond, today)
          {
            cf = bond@cash.flows
            if(is.null(today)) return(cf)
            else {
              time = index(cf)
              return(cf[time>=today])
            }
          }
          
          
)


setGeneric("getDiscountedCF", function(bond,r,today = NULL) {
  standardGeneric("getDiscountedCF")
})
setMethod("getDiscountedCF", "bond",
          function(bond,r, today)
          {
            cf = getCF(bond, today)
            cf.time = index(cf)
            
            if(is.null(today)) today = getIssueDate(bond)
            days = as.numeric(cf.time - today)/365
            return(cf*exp(-days*r))           
          }         
)





# Tests:
#bond = createBondSyn(face=20000,maturity=10,n.pay=1,coupon=0.05)
#getCF(bond)
#getCF(bond, as.Date("1973-12-31"))

#getDiscountedCF(bond,r=0.1)
#getDiscountedCF(bond,r = 0.1 ,as.Date("1973-12-31"))