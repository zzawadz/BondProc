setClass(Class="NS",representation=list(
  alpha1 = "numeric",
  alpha2 = "numeric",
  alpha3 = "numeric",
  beta   = "numeric"),
  prototype = list(
    alpha1 = 0.07,
    alpha2 = -0.04,
    alpha3 = -0.03,
    beta   = 1)
    
    )

#print(new("NS",0.07,-0.04,-0.03,1))

###################################################
# Tworzenie obiektu z parametrami Nelsona-Siegela #
###################################################
setMethod("initialize","NS",
          function(.Object,alpha1,alpha2,alpha3,beta)
          {
            .Object@alpha1 = alpha1
            .Object@alpha2 = alpha2
            .Object@alpha3 = alpha3
            .Object@beta = beta
            .Object
          }
)

newNS <-function(alpha1,alpha2,alpha3,beta)
{
  new("NS",alpha1,alpha2,alpha3,beta)
}



############################################
# Obliczanie stop z modelu Nelsona-Siegela #
############################################
setGeneric("NelsonSiegel", function(ns,t,...) {
  standardGeneric("NelsonSiegel")
})

setMethod("NelsonSiegel", "NS",
          function(ns,  t, ...)
          {
            alpha1 = ns@alpha1
            alpha2 = ns@alpha2
            alpha3 = ns@alpha3
            beta   = ns@beta
            alpha1+(alpha2+alpha3)*beta/t*(1-exp(-t/beta))-alpha3*exp(-t/beta)  
          }
)

# Test
# NelsonSiegel(new("NS"),5)

