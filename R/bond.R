setClass(Class="bond",representation=list(
  face = "numeric",
  maturity = "numeric",
  coupon = "numeric",
  n.pay   = "numeric",
  price = "numeric"),
         prototype = list(
           face = 100,
           maturity = 1,
           coupon = 0.1,
           n.pay   = 1,
           price = 0)
         
)

setMethod("initialize","bond",
          function(.Object,face,maturity,coupon,n.pay, price, r)
          {
            .Object@face = face
            .Object@maturity = maturity
            .Object@coupon = coupon
            .Object@n.pay = n.pay
            .Object@price = numeric()
            
            if(!is.null(price))
            {
              .Object@price = price
            }
            # jesli jest podana stopa procentowa, lub parametry N-S 
            # - wtedy obligacja jest od razu wyceniana
            if(!is.null(r))
            {
              .Object@price = bondPrice(r,.Object)
            }
            
            .Object
          }
)

newBond <-function(face,maturity,coupon,n.pay, price = NULL, r = NULL)
{
  new("bond",face,maturity,coupon,n.pay,price,r)
}


#Funkcja zwraca CASHFlow generowane przez obligacje

setGeneric("getCF", function(bond) {
  standardGeneric("getCF")
})
setMethod("getCF", "bond",
    function(bond)
    {
      if(length(bond@coupon)==1)
      {
        CF = rep(bond@coupon*bond@face/bond@n.pay,bond@maturity*bond@n.pay)
      } else 
      {
        CF = bond@coupon*bond@face
      }
      
      CF[length(CF)] = CF[length(CF)] + bond@face
      return(CF)
    }
)

setGeneric("getCFTime", function(bond) {
  standardGeneric("getCFTime")
})
setMethod("getCFTime", "bond",
          function(bond)
          {
            maturity = bond@maturity
            n.pay = bond@n.pay
            t = seq(1/n.pay,maturity,1/n.pay)
            return(t)
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

setMethod("getCFdisc", "NS",
          function(r,bond)
          {
            t = getCFTime(bond)
            r = NelsonSiegel(r,t)
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
