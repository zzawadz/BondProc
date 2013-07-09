# New - nie testowane!

setGeneric("getDuration", function(bond,r,today  = NULL,method = "FW",...) {
  standardGeneric("getDuration")
})

setMethod("getDuration", "bond",
          function(bond, r, today, method,...)
          {
            dCF = getDiscountedCF(bond, r, today)
            cf.time = getCFTime(bond,today)
            duration = sum(dCF*cf.time)/getBondPrice(bond,r,today,...)
            
            return(duration)
          }
)