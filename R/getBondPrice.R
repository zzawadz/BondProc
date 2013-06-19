
setGeneric("getBondPrice", function(bond,r=NULL,...) {
  standardGeneric("getBondPrice")
})

setMethod("getBondPrice", "bond",
          function(bond,r)
          {

            if(length(bond@price)==0)
            {
              if(is.null(r)) stop("Nie moge obliczyc ceny - nie podano struktury terminowej")
              return(bondPrice(r,bond))
            }
            else return(bond@price)
          }
)

setMethod("getBondPrice", "bondBasket",
          function(bond,r)
          {
            basket = bond
            basket.size = length(basket)
            prices = 1:basket.size
            
            for(i in 1:basket.size)
            {
              prices[i]   = getBondPrice(basket[[i]],r)
            }
            return(prices)
          }
)


# bond = newBond(face=100,maturity=10,coupon=0.1,n.pay=6)
# 
# # Valuation of bonds
# 
# # Using sigle value
# getBondPrice(bond,r= 0.1)
# 
# # Using multiple values
# r = seq(1,10,length.out=60)/100
# getBondPrice(bond,r)
# 
# # yield curve as a function
# rf <- function(t) (t*2)/100
# getBondPrice(bond,rf)
# 
# # Using Nelson-Siegel model
# ns = newNelsonSiegel(alpha1=0.06,0.03,0.02,1)
# getBondPrice(bond,ns)