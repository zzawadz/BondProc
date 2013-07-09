setGeneric("getBondPrice", function(bond,r=NULL, today = NULL,theoretical = FALSE) {
  standardGeneric("getBondPrice")
})

setMethod("getBondPrice", "bond",
          function(bond, r, today, theoretical)
          {

            if(length(bond@price)==0 | theoretical)
            {
              if(!theoretical) warning("There is no price for bond ISIN: ",getISIN(bond),". Theoretical price will be used.")
              
              if(is.null(r)) stop("There is no term structure. Price can not be calculated.")
              price = sum(getDiscountedCF(bond, r, today))
              return(price)
            }
            else return(bond@price)
          }
)

# bond = createBondSyn(face=10000,maturity=10,n.pay=4,coupon=0.05)
# getBondPrice(bond,0.1, theoretical=TRUE)
# getBondPrice(bond,0.1, as.Date("1973-12-31"))


setMethod("getBondPrice", "bondBasket",
          function(bond, r, today, theoretical)
          {
              sapply(bond,getBondPrice,r=r,today=today,theoretical=theoretical,simplify=TRUE)
          }
)

#basket = createBondSyn() + createBondSyn(face = 1000)
#getBondPrice(basket,0.1, theoretical=T)

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